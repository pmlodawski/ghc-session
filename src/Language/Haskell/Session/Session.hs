module Language.Haskell.Session.Session (
    -- * Session
    run,
    initialize,
    runWith,
    initializeWith,

    -- * Flags
    setStrFlags,

    -- * Imports
    setImports,
    withImports,

    -- * GHC extensions
    setFlags,
    unsetFlags,
    withExtensionFlags,

    -- * Expression evaluation
    runStmt,
    runDecls,
    runAssignment,
    runAssignment',
    interpret,

    -- * Context and DynFlags protection
    sandboxDynFlags,
    sandboxContext,
    interceptErrors
) where

import qualified Control.Exception      as Exception
import           Control.Monad
import           Control.Monad.Catch    (bracket)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.Ghc      (lift)
import qualified Control.Monad.Ghc      as MGHC
import           Control.Monad.IO.Class (liftIO)
import           Data.Typeable          (Typeable)
import qualified DynFlags
import qualified GHC
import qualified HscTypes
import qualified GHC.Paths  as Paths

import           Language.Haskell.Config            as Config
import qualified Language.Haskell.Session.Binding   as Binding
import qualified Language.Haskell.Session.Hint.Eval as HEval


type Session = MGHC.Ghc

type Import = String

--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------

-- | Run session with default config.
run :: Session a -> IO a
run session = MGHC.runGhc (Just Paths.libdir) $ initialize >> session


-- | Run session with custom GHC config.
runWith :: Config -> Session a -> IO a
runWith config session = MGHC.runGhc (Just $ Config.topDir config)
                       $ initializeWith config >> session


-- | Initialize session with custom GHC config.
initializeWith :: Config -> Session ()
initializeWith config = do
    initialize
    let globalPkgDb = Config.global $ Config.pkgDb config
        localPkgDb  = Config.local  $ Config.pkgDb config
        isNotUser DynFlags.UserPkgConf = False
        isNotUser _ = True
        extraPkgConfs p = [ DynFlags.PkgConfFile globalPkgDb
                          , DynFlags.PkgConfFile localPkgDb
                          ] ++ filter isNotUser p
    flags <- GHC.getSessionDynFlags
    void  $  GHC.setSessionDynFlags flags
                { GHC.extraPkgConfs = extraPkgConfs
                --, GHC.verbosity = 4
                }

-- | Initialize session with default config.
initialize :: Session ()
initialize = do
    setStrFlags ["-fno-ghci-sandbox"]
    flags <- GHC.getSessionDynFlags
    void  $  GHC.setSessionDynFlags flags
                { GHC.hscTarget     = GHC.HscInterpreted
                , GHC.ghcLink       = GHC.LinkInMemory
                , GHC.ctxtStkDepth  = 1000
                --, GHC.verbosity = 4
                }

--------------------------------------------------------------------------------
-- Flags
--------------------------------------------------------------------------------

-- | Set ghc command line arguments.
setStrFlags :: [String] -> Session ()
setStrFlags strFlags = do
    flags <- GHC.getInteractiveDynFlags
    (flags2, leftovers, warns) <- GHC.parseDynamicFlags flags $ map GHC.noLoc strFlags
    liftIO $ HscTypes.handleFlagWarnings flags2 warns
    let unrecognized = map (show . GHC.unLoc) leftovers
    unless (null unrecognized) $
        fail $ "Unrecognized flags: " ++ unwords unrecognized
    void $ GHC.setInteractiveDynFlags flags2

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- | Set imports and replace existing ones.
setImports :: [Import] -> Session ()
setImports = GHC.setContext . map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName)


-- | Run a code with temporary imports.
withImports :: [Import] -> Session a -> Session a
withImports imports action = sandboxContext $ do
    setImports imports
    action


location :: String
location = "<target ghc-hs interactive>"

--------------------------------------------------------------------------------
-- GHC extensions
--------------------------------------------------------------------------------

-- | Set GHC extension flags.
setFlags :: [DynFlags.ExtensionFlag] -> Session ()
setFlags flags = do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl DynFlags.xopt_set current flags

-- | Unset GHC extension flags.
unsetFlags :: [DynFlags.ExtensionFlag] -> Session ()
unsetFlags flags = do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl DynFlags.xopt_unset current flags

-- | Run a code with temporary GHC extension flags.
withExtensionFlags :: [DynFlags.ExtensionFlag] -> [DynFlags.ExtensionFlag] -> Session a -> Session a
withExtensionFlags enable disable action = sandboxDynFlags $ do
    setFlags enable
    unsetFlags disable
    action

--------------------------------------------------------------------------------
-- Expression evaluation
--------------------------------------------------------------------------------

-- | Run statement as in ghci.
runStmt :: String -> Session ()
runStmt stmt = do
    result <- interceptErrors $ GHC.runStmtWithLocation location 1 stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> fail $ "runStmt : " ++ show ex
        GHC.RunBreak {}     -> fail $ "runStmt : RunBreak"

-- | Run declaration as in Haskell source file.
runDecls :: String -> Session ()
runDecls decls = do
    void $ interceptErrors $ GHC.runDeclsWithLocation location 1 decls


-- | Bind expression do a variable using `let` syntax.
runAssignment :: String -> String -> Session ()
runAssignment asigned asignee = do
    Binding.removeBinding asigned
    -- do not use runDecls here: its bindings are hard to remove and cause memory leaks!
    runStmt $ "let " ++ asigned ++ " = " ++ asignee

-- | Bind expression to a variable using `bind` syntax. Expression must have type `IO a`.
runAssignment' :: String -> String -> Session ()
runAssignment' asigned asignee = do
    Binding.removeBinding asigned
    runStmt $ asigned ++ " <- " ++ asignee


-- | Evaluate expression.
interpret :: Typeable a => String -> Session a
interpret = interceptErrors . HEval.interpret

--------------------------------------------------------------------------------
-- Context and DynFlags protection
--------------------------------------------------------------------------------

-- | Run a code which can safely modify DynFlags - it will be restored on exit.
sandboxDynFlags :: Session a -> Session a
sandboxDynFlags = bracket GHC.getSessionDynFlags GHC.setSessionDynFlags . const


-- | Run a code which can safely modify Context w- it  will be restored on exit.
sandboxContext :: Session a -> Session a
sandboxContext = bracket GHC.getContext GHC.setContext . const


-- | Restore session when exception will be raised.
interceptErrors :: MGHC.Ghc a -> Session a
interceptErrors ghc = do
    sessionBackup <- GHC.getSession
    let handler :: Catch.SomeException -> MGHC.Ghc a
        handler otherErr = do
            GHC.setSession sessionBackup
            Exception.throw otherErr
    Catch.catch ghc handler
