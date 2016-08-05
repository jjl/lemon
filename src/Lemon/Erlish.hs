module Lemon.Erlish where

import Data.Scientific (floatingOrInteger)
import Lemon.Erlish.Monad
import qualified Lemon.AST as A
import Lemon.Erlish.Data as E
import Lemon.Erlish.Prim as Prim

erlify :: A.Node -> E.Term
erlify (A.Number n) = either E.Float E.Int (floatingOrInteger n)
erlify (A.Char c)   = E.Int (fromIntegral $ fromEnum c)
erlify (A.String s) = E.Binary s
erlify (A.Atom s)   = E.Atom s
erlify (A.List l)   = E.List (fmap erlify l)

macroexpand1 :: E.Term -> Erlish
macroexpand1 f@(E.List ((E.Atom s):_)) = findMacro s >>= maybe (return f) ($ f)
macroexpand1 f = return f

macroexpand :: E.Term -> Erlish
macroexpand f = macroexpand1 f >>= \r -> if f == r then return f else macroexpand r

macroexpandAll :: E.Term -> Erlish
macroexpandAll f = macroexpand f >>= \f2 ->
  case f2 of
    (E.List (h:t)) -> scoped $
      do h2 <- scoped $ macroexpandAll h
         rs <- (macroexpandAll $ E.List t)
         cons h2 rs
    _ -> make f2

-- eval :: E.Term -> Erlish
-- eval (E.List l) = ...
-- eval t = t -- Anything else evaluates to itself

