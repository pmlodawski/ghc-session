{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Session.Hint.Typecheck where

import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class (liftIO)
import qualified GHC

import qualified Language.Haskell.Session.Hint.Conversions as Conversions


-- | Get string representation of expression type
typeOf :: GHC.GhcMonad m => String -> m String
typeOf = GHC.exprType >=> Conversions.typeToString

-- | Dump type of an expression
debugType :: GHC.GhcMonad m => String -> m ()
debugType v = do
    t <- typeOf v
    liftIO $ putStrLn $ v ++ "\n    :: " ++ t
