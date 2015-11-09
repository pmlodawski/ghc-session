module Main where

import qualified Language.Haskell.Session as Session
import Control.Monad.IO.Class (liftIO)



main = Session.run $ do
    Session.setImports ["Prelude"]
    Session.runAssignment "x" "5"
    Session.runAssignment "y" "\"bar\""
    foo <- Session.interpret "print (x, y)"
    x <- Session.interpret "x"--(x, y)"
    liftIO $ print (x :: Integer)
    liftIO $ (foo :: IO ())

    Session.runDecls "foo = print (x, y)"
    foo <- Session.interpret "foo"
    liftIO $ (foo :: IO ())

    Session.runAssignment "bar" "print (y, x)"
    bar <- Session.interpret "bar"
    liftIO $ (bar :: IO ())

    Session.runStmt "bar"
    Session.runStmt "print 555"
    Session.removeBinding "x"
    -- Session.runStmt "print x"

    Session.runDecls "data A = A deriving Show"
    Session.runStmt "print A"
