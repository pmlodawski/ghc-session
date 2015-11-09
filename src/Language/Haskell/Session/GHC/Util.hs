module Language.Haskell.Session.GHC.Util where

import qualified GHC
import           Outputable (Outputable)
import qualified Outputable



dshow :: Outputable a => GHC.DynFlags -> a -> String
dshow dflags = Outputable.showSDoc dflags . Outputable.ppr
