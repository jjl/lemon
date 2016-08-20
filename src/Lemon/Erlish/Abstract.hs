{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
module Lemon.Erlish.Abstract where

import Control.Monad.Freer.Exception
import Control.Monad.Freer.Internal
import Data.Text as T
import Data.Map.Strict as M
import Lemon.Erlish.Data as E
import Prelude as P hiding (error)

type Errory r v w = Member (Exc (Error (Term v w))) r => Arr r (Term v w) (Term v w)

error :: WithError r v w => Text -> Text -> Arr' r (Term v w)
error n m f = throwError (InvalidArgument msg f)
  where msg = T.intercalate ": " [m, T.pack $ show n]

withL :: WithError r v w => Text -> Arr r [Term v w] (Term v w) -> Arr' r (Term v w)
withL _ f (E.List l _) = f l
withL n _ v = error n "called incorrectly using the internal api" v

with1 :: WithError r v w => Text -> (Arr' r (Term v w)) -> Arr' r (Term v w)
with1 _ f (E.List [h] _) = f h
with1 n _ v@(E.List _ _) = error n "expected 1 arg" v
with1 n _ v = error n "called incorrectly using the internal api" v

with1L :: WithError r v w => Text -> (Term v w -> Arr r [Term v w] (Term v w)) -> Arr' r (Term v w)
with1L _ f (E.List (h:t) _) = f h t
with1L n _ v@(E.List _ _)   = error n "expected at least 1 arg" v
with1L n _ v = error n "called incorrectly using the internal api" v

with2 :: WithError r v w => Text -> (Term v w -> Arr' r (Term v w)) -> Arr' r (Term v w)
with2 _ f (E.List [h,i] _) = f h i
with2 n _ v@(E.List _ _) = error n "expected 2 args" v
with2 n _ v = error n "called incorrectly using the internal api" v

with2L :: WithError r v w =>
          Text -> (Term v w -> Term v w -> Arr r [Term v w] (Term v w))
          -> Arr' r (Term v w)
with2L _ f (E.List (h:i:t) _) = f h i t
with2L n _ v@(E.List _ _)     = error n "expected at least 2 args" v
with2L n _ v = error n "called incorrectly using the internal api" v

with3 :: WithError r v w
         => Text -> (Term v w -> Term v w -> Arr' r (Term v w))
         -> Arr' r (Term v w)
with3 _ f (E.List [h,i,j] _) = f h i j
with3 n _ v@(E.List _ _) = error n "expected 3 args" v
with3 n _ v = error n "called incorrectly using the internal api" v

with3L :: WithError r v w
          => Text -> (Term v w -> Term v w -> Term v w -> Arr r [Term v w] (Term v w))
          -> Arr' r (Term v w)
with3L _ f (E.List (h:i:j:t) _) = f h i j t
with3L n _ v@(E.List _ _) = error n "expected at least 3 args" v
with3L n _ v = error n "called incorrectly using the internal api" v

-- changeMap :: Text -> (Map Term Term -> [Term] -> Lemonade (Map Term Term)) -> E.Term -> Erlish
-- changeMap n f = with3L n $ \m k v r -> case m of
--    Map m2 -> if 0 == (P.length r) `mod` 2 then fmap E.Map (f m2 $ k:v:r)
--              else throwE (InvalidArgument "map-set expects an odd number of arguments" $ E.List r)
--    _ -> error n "expects a map as argument 1" m

-- helpMap :: Text -> Map Term Term -> [Term] -> (Map Term Term -> Term -> Term -> [Term] -> Lemonade (Map Term Term)) -> Lemonade (Map Term Term)
-- helpMap _ m [] _ = return m
-- helpMap _ m (k:v:r) f = f m k v r
-- helpMap n _ e _ = error n "unexpected arguments" (E.List e)
-- 
