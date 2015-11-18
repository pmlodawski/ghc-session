module Language.Haskell.Session.Hint.Util where

import qualified Data.Char as Char


type Expr = String

-- @safeBndFor expr@ generates a name @e@ such that it does not
-- occur free in @expr@ and, thus, it is safe to write something
-- like @e = expr@ (otherwise, it will get accidentally bound).
-- This ought to do the trick: observe that @safeBndFor expr@
-- contains more digits than @expr@ and, thus, cannot occur inside
-- @expr@.
safeBndFor :: Expr -> String
safeBndFor expr = "e_1" ++ filter Char.isDigit expr
