---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

module Language.Haskell.Session.Hint.Conversions where

import qualified DynFlags
import qualified GHC
import qualified Outputable
import qualified Type



typeToString :: GHC.GhcMonad m => GHC.Type -> m String
typeToString t = do
    -- Unqualify necessary types
    -- (i.e., do not expose internals)
    unqual <- GHC.getPrintUnqual
    df <- DynFlags.getDynFlags
    return $ Outputable.showSDocForUser df unqual (Type.pprType t)
