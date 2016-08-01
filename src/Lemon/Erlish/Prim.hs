{-# LANGUAGE OverloadedStrings #-}
module Lemon.Erlish.Prim where

import Control.Monad.Trans.Except
import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E

car, cdr :: E.Term -> Erlish
car (E.List (h:_)) = return h
car f = throwE (InvalidArgument "car requires a non-empty list" f)

cdr (E.List (_:t)) = return $ E.List t
cdr f = throwE (InvalidArgument "cdr requires a non-empty list" f)

cons :: E.Term -> E.Term -> Erlish
cons i (E.List l) = return (E.List $ i:l)
cons i f = throwE (InvalidArgument "cons requires a list as second argument" f)

-- cadr :: E.Term -> Erlish
-- cadr t = cdr t >>= \t2 -> return $ car t2
