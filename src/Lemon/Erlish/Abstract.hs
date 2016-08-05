{-# LANGUAGE OverloadedStrings #-}
module Lemon.Erlish.Abstract where

import Control.Monad.Trans.Except
import Data.Text as T
import Data.Map.Strict as M
import Lemon.Erlish.Data as E
import Prelude as P hiding (error)

error :: Text -> Text -> Term -> Lemonade a
error n m f = throwE (InvalidArgument msg f)
  where msg = T.intercalate ": " [m, T.pack $ show n]

withL :: Text -> ([Term] -> Erlish) -> Term -> Erlish
withL _ f (E.List l) = f l
withL n _ v = error n "called incorrectly using the internal api" v

with1 :: Text -> (Term -> Erlish) -> Term -> Erlish
with1 _ f (E.List (h:_)    ) = f h
with1 n _ v@(E.List _) = error n "expected 1 arg" v
with1 n _ v = error n "called incorrectly using the internal api" v

with1L :: Text -> (Term -> [Term] -> Erlish) -> Term -> Erlish
with1L _ f (E.List (h:t)    ) = f h t
with1L n _ v@(E.List _) = error n "expected at least 1 arg" v
with1L n _ v = error n "called incorrectly using the internal api" v

with2 :: Text -> (Term -> Term -> Erlish) -> Term -> Erlish
with2 _ f (E.List (h:i:_)  ) = f h i
with2 n _ v@(E.List _) = error n "expected 2 args" v
with2 n _ v = error n "called incorrectly using the internal api" v

with2L :: Text -> (Term -> Term -> [Term] -> Erlish) -> Term -> Erlish
with2L _ f (E.List (h:i:t)  ) = f h i t
with2L n _ v@(E.List _) = error n "expected at least 2 args" v
with2L n _ v = error n "called incorrectly using the internal api" v

with3 :: Text -> (Term -> Term -> Term -> Erlish) -> Term -> Erlish
with3 _ f (E.List (h:i:j:_)) = f h i j
with3 n _ v@(E.List _) = error n "expected 3 args" v
with3 n _ v = error n "called incorrectly using the internal api" v

with3L :: Text -> (Term -> Term -> Term -> [Term] -> Erlish) -> Term -> Erlish
with3L _ f (E.List (h:i:j:t)) = f h i j t
with3L n _ v@(E.List _) = error n "expected at least 3 args" v
with3L n _ v = error n "called incorrectly using the internal api" v

changeMap :: Text -> (Map Term Term -> [Term] -> Lemonade (Map Term Term)) -> E.Term -> Erlish
changeMap n f = with3L n $ \m k v r -> case m of
   Map m2 -> if 0 == (P.length r) `mod` 2 then fmap E.Map (f m2 $ k:v:r)
             else throwE (InvalidArgument "map-set expects an odd number of arguments" $ E.List r)
   _ -> error n "expects a map as argument 1" m

helpMap :: Text -> Map Term Term -> [Term] -> (Map Term Term -> Term -> Term -> [Term] -> Lemonade (Map Term Term)) -> Lemonade (Map Term Term)
helpMap _ m [] _ = return m
helpMap _ m (k:v:r) f = f m k v r
helpMap n _ e _ = error n "unexpected arguments" (E.List e)

