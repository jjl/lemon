{-# LANGUAGE OverloadedStrings #-}
module Lemon.Erlish.Prim where

import Control.Monad.Trans.Except
import Lemon.Erlish.Abstract
import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E
import Data.Map.Strict as M (Map, empty, lookup, insert)
import Prelude hiding (map, error)
import qualified Prelude as P



-- car, cdr :: Term -> Erlish
-- car = with1 "car" $ \x -> case x of
--   E.List (h:_) -> return h
--   f -> throwE (InvalidArgument "car requires a non-empty list" f)
-- cdr = with1 "cdr" $ \x -> case x of
--   E.List (_:t) -> return $ E.List t
--   f -> throwE (InvalidArgument "cdr requires a non-empty list" f)
-- caar, cadr, cddr, cdar :: Term -> Erlish
-- caar t = car t >>= car
-- cadr t = cdr t >>= car
-- cddr t = cdr t >>= cdr
-- cdar t = car t >>= cdr
-- caaar, caadr, cadar, caddr, cdddr, cddar, cdaar, cdadr :: Term -> Erlish
-- caaar t = caar t >>= car
-- caadr t = cadr t >>= car
-- cadar t = cdar t >>= car
-- caddr t = cddr t >>= car
-- cdddr t = cddr t >>= cdr
-- cddar t = cdar t >>= cdr
-- cdaar t = caar t >>= cdr
-- cdadr t = cadr t >>= cdr
-- caaaar, caadar, cadaar, cadadr, caddar, cdaddr, cdaadr, cddadr, caaadr, caaddr, cadddr, cddddr, cdddar, cddaar, cdaaar, cdadar :: Term -> Erlish
-- caaaar t = caaar t >>= car
-- caaadr t = caadr t >>= car
-- caadar t = cadar t >>= cdr
-- caaddr t = caddr t >>= car
-- cadaar t = cdaar t >>= car
-- cadadr t = cdadr t >>= car
-- caddar t = cddar t >>= car
-- cadddr t = cdddr t >>= car
-- cdaaar t = caaar t >>= cdr
-- cdadar t = cadar t >>= cdr
-- cdaddr t = caddr t >>= cdr
-- cdaadr t = caadr t >>= cdr
-- cdddar t = cddar t >>= cdr
-- cddddr t = cdddr t >>= cdr
-- cddaar t = cdaar t >>= cdr
-- cddadr t = cdadr t >>= cdr

-- cons :: E.Term -> Erlish
-- cons = with2 "cons" $ \x y -> case y of
--   E.List l -> return (E.List $ x:l)
--   f -> throwE (InvalidArgument "cons requires a list as second argument" f)

-- quote :: E.Term -> Erlish
-- quote = with1 "quote" make

-- list :: E.Term -> Erlish
-- list = withL "list" (return . E.List)

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

