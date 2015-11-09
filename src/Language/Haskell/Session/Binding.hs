module Language.Haskell.Session.Binding (
    removeBinding
) where

import qualified Data.List as List
import qualified GHC
import           GhcMonad  (GhcMonad)
import qualified GhcMonad
import qualified HscTypes
import qualified Linker

import Language.Haskell.Session.GHC.Util (dshow)



bindingMatch :: GHC.DynFlags -> String -> HscTypes.TyThing -> Bool
bindingMatch dflags name binding = result where
    result = dropDot bname == name
    bname = dshow dflags $ HscTypes.tyThingAvailInfo binding


dropDot :: String -> String
dropDot = reverse . List.takeWhile (/= '.') . reverse


removeBinding :: GhcMonad m => String -> m ()
removeBinding name = do
    dflags <- GHC.getSessionDynFlags
    matching <- filter (bindingMatch dflags name) <$> getIcTythings
    GhcMonad.liftIO $ Linker.deleteFromLinkEnv $ map GHC.getName matching
    removeIcTythings name


getIcTythings :: GhcMonad m => m [HscTypes.TyThing]
getIcTythings = do
    hscEnv <- GhcMonad.getSession
    return $ HscTypes.ic_tythings $ HscTypes.hsc_IC hscEnv


getBindings :: GhcMonad m => m [String]
getBindings = do
    dflags <- GHC.getSessionDynFlags
    map (dropDot . dshow dflags . HscTypes.tyThingAvailInfo) <$> getIcTythings


removeIcTythings :: GhcMonad m => String -> m ()
removeIcTythings name = do
    dflags <- GHC.getSessionDynFlags
    GhcMonad.modifySession $ \hscEnv -> let
        hsc_IC       = HscTypes.hsc_IC       hscEnv
        ic_tythings  = HscTypes.ic_tythings hsc_IC
        ic_tythings' = filter (not . bindingMatch dflags name) ic_tythings
        hsc_IC'      = hsc_IC {HscTypes.ic_tythings = ic_tythings'}
        in hscEnv { HscTypes.hsc_IC = hsc_IC'}
