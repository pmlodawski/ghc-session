---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Session.Hint.Typecheck where

import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class (liftIO)
import qualified GHC

import qualified Language.Haskell.Session.Hint.Conversions as Conversions



typeOf :: GHC.GhcMonad m => String -> m String
typeOf = GHC.exprType >=> Conversions.typeToString


debugType :: GHC.GhcMonad m => String -> m ()
debugType v = do
    t <- typeOf v
    liftIO $ putStrLn $ v ++ "\n    :: " ++ t
