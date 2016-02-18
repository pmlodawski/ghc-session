{-# LANGUAGE ConstraintKinds #-}

module Language.Haskell.Session.Session (
    -- * Session
    SessionMonad,
    run,
    runWith,

    -- * Flags
    setStrFlags,

    -- * Imports
    Import,
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
import           GHC                    (GhcMonad)
import qualified GHC
import qualified GHC.Paths              as Paths
import qualified HscTypes

import           Language.Haskell.Config            as Config
import qualified Language.Haskell.Session.Binding   as Binding
import qualified Language.Haskell.Session.Hint.Eval as HEval


type SessionMonad m = (GhcMonad m, Catch.MonadMask m, Catch.MonadCatch m)

type Import = String

--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------

-- | Run a session with default config.
run :: (MGHC.MonadIO m, Catch.MonadMask m, Functor m) => MGHC.GhcT m a -> m a
run session = MGHC.runGhcT (Just Paths.libdir) $ initialize >> session


-- | Run a session with custom GHC config.
runWith :: Config -> MGHC.Ghc a -> IO a
runWith config session = MGHC.runGhc (Just $ Config.topDir config)
                       $ initializeWith config >> session


-- | Initialize a session with custom GHC config.
initializeWith :: GhcMonad m => Config -> m ()
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

-- | Initialize a session with default config.
initialize :: GhcMonad m => m ()
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
setStrFlags :: GhcMonad m => [String] -> m ()
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
setImports :: GhcMonad m => [Import] -> m ()
setImports = GHC.setContext . map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName)


-- | Run a code with temporary imports.
withImports :: (Catch.MonadMask m, GhcMonad m) => [Import] -> m a -> m a
withImports imports action = sandboxContext $ do
    setImports imports
    action


location :: String
location = "<target ghc-hs interactive>"

--------------------------------------------------------------------------------
-- GHC extensions
--------------------------------------------------------------------------------

-- | Set GHC extension flags.
setFlags :: GhcMonad m => [DynFlags.ExtensionFlag] -> m ()
setFlags flags = do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl DynFlags.xopt_set current flags

-- | Unset GHC extension flags.
unsetFlags :: GhcMonad m => [DynFlags.ExtensionFlag] -> m ()
unsetFlags flags = do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl DynFlags.xopt_unset current flags

-- | Run a code with temporary GHC extension flags.
withExtensionFlags :: (Catch.MonadMask m, GhcMonad m) => [DynFlags.ExtensionFlag] -> [DynFlags.ExtensionFlag] -> m a -> m a
withExtensionFlags enable disable action = sandboxDynFlags $ do
    setFlags enable
    unsetFlags disable
    action

--------------------------------------------------------------------------------
-- Expression evaluation
--------------------------------------------------------------------------------

-- | Run a statement as in ghci.
runStmt :: (Catch.MonadCatch m, GhcMonad m) => String -> m ()
runStmt stmt = do
    result <- interceptErrors $ GHC.runStmtWithLocation location 1 stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> fail $ "runStmt : " ++ show ex
        GHC.RunBreak {}     -> fail $ "runStmt : RunBreak"

-- | Run declarations as in Haskell source file.
runDecls :: (Catch.MonadCatch m, GhcMonad m) => String -> m ()
runDecls decls = do
    void $ interceptErrors $ GHC.runDeclsWithLocation location 1 decls


-- | Bind an expression to a variable using `let` syntax.
runAssignment :: (Catch.MonadCatch m, GhcMonad m) => String -> String -> m ()
runAssignment asigned asignee = do
    Binding.removeBinding asigned
    -- do not use runDecls here: its bindings are hard to remove and cause memory leaks!
    runStmt $ "let " ++ asigned ++ " = " ++ asignee

-- | Bind an expression to a variable using `bind` syntax. Expression must have type `IO a`.
runAssignment' :: (Catch.MonadCatch m, GhcMonad m) => String -> String -> m ()
runAssignment' asigned asignee = do
    Binding.removeBinding asigned
    runStmt $ asigned ++ " <- " ++ asignee


-- | Evaluate an expression.
interpret :: (Catch.MonadCatch m, GhcMonad m) => Typeable a => String -> m a
interpret = interceptErrors . HEval.interpret

--------------------------------------------------------------------------------
-- Context and DynFlags protection
--------------------------------------------------------------------------------

-- | Run a code which can safely modify DynFlags - it will be restored on exit.
sandboxDynFlags :: (Catch.MonadMask m, GhcMonad m) => m a -> m a
sandboxDynFlags = bracket GHC.getSessionDynFlags GHC.setSessionDynFlags . const


-- | Run a code which can safely modify Context - it  will be restored on exit.
sandboxContext :: (Catch.MonadMask m, GhcMonad m) => m a -> m a
sandboxContext = bracket GHC.getContext GHC.setContext . const


-- | Restore session when exception will be raised.
interceptErrors :: (Catch.MonadCatch m, GhcMonad m) => m a -> m a
interceptErrors ghc = do
    sessionBackup <- GHC.getSession
    let handler ::(Catch.MonadCatch m, GhcMonad m) => Catch.SomeException -> m a
        handler otherErr = do
            GHC.setSession sessionBackup
            Exception.throw otherErr
    Catch.catch ghc handler
