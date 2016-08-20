{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module Lemon.Erlish.Prim where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State
import Data.Map.Strict as M (Map, empty, lookup, insert)
import Lemon.Erlish.Abstract
-- import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E
import Lemon.Util as U
import Prelude hiding (map, error)
import qualified Prelude as P

car', cdr' :: Errory r v w
car' (E.List (h:_) _) = return h
car' t = throwError (InvalidArgument "car requires a non-empty list" t)
cdr' (E.List (_:t) w) = return (E.List t w)
cdr' t = throwError (InvalidArgument "cdr requires a non-empty list" t)

car,cdr :: Errory r v w
car = with1 "car" car'
cdr = with1 "cdr" cdr'

caar, cadr, cddr, cdar :: Errory r v w
caar t = car t >>= car
cadr t = cdr t >>= car
cddr t = cdr t >>= cdr
cdar t = car t >>= cdr

caaar, caadr, cadar, caddr, cdddr, cddar, cdaar, cdadr :: Errory r v w
caaar t = caar t >>= car
caadr t = cadr t >>= car
cadar t = cdar t >>= car
caddr t = cddr t >>= car
cdddr t = cddr t >>= cdr
cddar t = cdar t >>= cdr
cdaar t = caar t >>= cdr
cdadr t = cadr t >>= cdr

caaaar, caadar, cadaar, cadadr, caddar, cdaddr, cdaadr, cddadr, caaadr, caaddr, cadddr, cddddr, cdddar, cddaar, cdaaar, cdadar :: Errory r v w
caaaar t = caaar t >>= car
caaadr t = caadr t >>= car
caadar t = cadar t >>= cdr
caaddr t = caddr t >>= car
cadaar t = cdaar t >>= car
cadadr t = cdadr t >>= car
caddar t = cddar t >>= car
cadddr t = cdddr t >>= car
cdaaar t = caaar t >>= cdr
cdadar t = cadar t >>= cdr
cdaddr t = caddr t >>= cdr
cdaadr t = caadr t >>= cdr
cdddar t = cddar t >>= cdr
cddddr t = cdddr t >>= cdr
cddaar t = cdaar t >>= cdr
cddadr t = cdadr t >>= cdr

cons' :: Term v w -> Errory r v w
cons' x (List l w) = return (E.List (x:l) w)
cons' _ t = throwError (InvalidArgument "cons requires a list as second argument" t)

cons :: Errory r v w
cons = with2 "cons" cons'

quote :: Errory r v w
quote = with1 "quote" return

_export :: WithError r v w => Term v w -> Arr' r (Module v w)
_export l@(E.List [_] _) _ = throwError (InvalidArgument msg l)
  where msg = "export requires arguments. valid: * (foo 1 2) (foo *)"
-- _export l@(E.List [_,(E.Atom "*" _)] _) m = 
-- _export l@(E.List (_:is) _) m = ... -- 
-- _exportMacros :: WithError r v w => Term v w -> Arr' r (Module v w)
-- _exportMacros l@(E.List [] w) _ = throwError (InvalidArgument msg (E.List [(E.Atom "export-macros" w)] w))
--   where msg = "export requires arguments. valid: * (foo 1 2) (foo *)"

_dmList :: WithError r v w => Term v w -> Arr' r (Module v w)
_dmList l@(E.List ((E.Atom "export" _):ts) _) m = _export l m
--_dmList (E.List ((E.Atom "export-macros" _):ts) w) m = _export (E.List ts w) m

-- Updates the module with a new piece of info from the defmodule form
-- TODO: FIXME
_dm :: WithError r v w => Term v w -> Arr' r (Module v w)
-- _dm (E.List   l w) m =
-- _dm (E.Map n w) m =
-- _dm (E.Binary b w) m =
_dm t _ = throwError (InvalidArgument "defmodule: expected list, map or binary" t)

-- defmodule' :: forall r v w. Arr r [Term v w] (Term v w)
-- defmodule' ((Atom a aw):rs) | validModuleName a =
--   do let m = Module a M.empty (ModScope M.empty M.empty)
     
-- defmodule' t = throwError (InvalidArgument "The first argument to defmodule must be a valid module atom" t)

-- defineMacro :: Text -> META
--defineMacro name

-- defmacro' :: forall r v w. Arr r [Term v w] (Term v w)
-- defmacro' ((E.Atom a w):(E.List l _):(E.Binary b _):exprs) =
--   do return (E.List [(E.Atom a w)] w)

-- evalAll :: [Term v w] -> w -> Eff w (Term v w)
-- evalAll ts w = mapM eval ts

-- -- evalList :: forall v w. [Term v w] -> w -> Eff w (Term v w)
-- -- evalList ts w =
-- --   do let (h:t) = mapM eval ts 
-- --      (Atom a w) <- findMacro @v @w h
     
     

-- eval :: Arr r (Term v w) (Term v w)
-- eval (List l w) = evalAll l w >>= evalList

-- list :: Errory r v w
-- list = withL "list" fmap E.List (mapM eval)

-- tuple :: E.Term -> Erlish
-- tuple = withL "tuple" (return . E.Tuple)

-- map :: E.Term -> Erlish
-- map = withL "map" $ \xs ->
--     if 0 == (length xs `mod` 2) then return (E.Map $ h M.empty xs)
--     else throwE (InvalidArgument "map expects an even number of arguments" $ E.List xs)
--   where h m (a:b:c) = h (insert a b m) c
--         h m [] = m
--         h _ _ = P.error "shut up ghc"

-- mget :: E.Term -> Erlish
-- mget = with2 "mget" $ \m k -> case m of
--   E.Map m2 -> case M.lookup k m2 of
--     Just j -> return j
--     _ -> error "mget" "key not in map" k
--   _ -> error "mget" "expects a map as arg 1" m

-- mset :: E.Term -> Erlish
-- mset = changeMap "mset" (\m ts -> helpMap "mset" m ts help)
--   where help m k v r = helpMap "mset" (M.insert k v m) r help

-- mup :: E.Term -> Erlish
-- mup = changeMap "mup" (\m ts -> helpMap "mup" m ts help)
--   where help m k v r = case M.lookup k m of
--           Just _ -> helpMap "mup" (M.insert k v m) r help
--           _ -> throwE(InvalidArgument "key not in map" k)

-- fn :: E.Term -> Erlish
-- fn = withL $ \x -> case x of
--   (x:y:z)
  
-- mfn :: E.Term -> Erlish
-- mfn = delist' $ \x ->

-- let
-- let-fn (letrec-fn)
-- let-macro
-- do
-- if
-- case
-- receive


-- call func arg
-- call (mod func) arg...

-- evalList :: [Term] -> Erlish
-- evalList [] = E.List []
-- evalList (h:t) =
--   do h2 <- eval h
     
--   Atom a -> ...
--   Fun f -> ...

-- eval :: Term -> Erlish
-- eval (List l) = evalList m
-- eval (Map m) =
-- eval o = o
-- data Term = Int    { _int   :: Integer }
--           | Float  { _float :: Double }
--           | Atom   { _atom  :: Text }
--           | Fun    { _fun   :: Func }
--           | Tuple  { _items :: [Term] }
--           | List   { _items :: [Term] }
--           | Binary { _bin   :: Text }
--           | Map    { _map   :: (M.Map Term Term) }
--             deriving (Eq, Show)

