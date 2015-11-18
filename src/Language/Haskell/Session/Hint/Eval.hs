{-# LANGUAGE MagicHash #-}

module Language.Haskell.Session.Hint.Eval (
    interpret,
    interpretTyped,
) where

import           Data.Typeable (Typeable)
import qualified Data.Typeable as Typeable
import qualified GHC
import qualified GHC.Exts      as Exts

import qualified Language.Haskell.Session.Hint.Util as Util



interpret :: (GHC.GhcMonad m, Typeable a) => String -> m a
interpret expr = interpret' expr wit where
    wit :: a
    wit = undefined


-- | Evaluates an expression, given a witness for its monomorphic type.
interpret' :: (GHC.GhcMonad m, Typeable a) => String -> a -> m a
interpret' expr wit = interpretTyped expr typeStr where
    typeStr = show $ Typeable.typeOf wit


interpretTyped :: GHC.GhcMonad m => String -> String -> m a
interpretTyped expr typeStr = do
    let typedExpr = concat [parens expr, " :: ", typeStr]
    exprVal <- GHC.compileExpr typedExpr
    return (Exts.unsafeCoerce# exprVal :: a)


-- | Conceptually, @parens s = \"(\" ++ s ++ \")\"@, where s is any valid haskell
-- expression. In practice, it is harder than this.
-- Observe that if @s@ ends with a trailing comment, then @parens s@ would
-- be a malformed expression. The straightforward solution for this is to
-- put the closing parenthesis in a different line. However, now we are
-- messing with the layout rules and we don't know where @s@ is going to
-- be used!
-- Solution: @parens s = \"(let {foo =\n\" ++ s ++ \"\\n ;} in foo)\"@ where @foo@ does not occur in @s@
parens :: String -> String
parens s = concat ["(let {", foo, " =\n", s, "\n",
                    "                     ;} in ", foo, ")"]
    where foo = Util.safeBndFor s
