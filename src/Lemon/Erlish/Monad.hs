{-# LANGUAGE FlexibleInstances, OverloadedStrings, TemplateHaskell, TypeSynonymInstances #-}
module Lemon.Erlish.Monad
  ( Set, Over, Put -- type aliases
  , make
  , getGsc, setGsc, overGsc
  , getScopes, setScopes, overScopes, eitherScopes
  , enscope, descope, unscope, scoped
  , currentScope, overCurrentScope
  , findPrim, findScope, findMacro, findFun, findBind
  , putMacro, putFun, putBind
  ) where

import Control.Lens hiding (Over)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Data.Map.Strict as S
import Data.Maybe as M
import Data.Text (Text)
import Lemon.Erlish.Data as E

-- When life gives you lemons, make Lemonade
make :: a -> Lemonade a
make f = lift (return f)

-- _get  :: LENS -> Lemonade a
-- _set  :: LENS -> a -> Lemonade ()
-- _over :: LENS -> (a -> a) -> Lemonade ()
_get l = fmap (^. l) (lift get)
_set l v = lift get >>= (\st -> lift $ put (set l v st))
_over l f = lift $ modify (over l f)

type Set  a = a -> Lemonade ()
type Over a = Set (a -> a)
type Find a = Text -> Lemonade (Maybe a)
type Put a = Text -> a -> Lemonade ()

getGsc :: Lemonade Int
getGsc = _get E.gsc
setGsc :: Set Int
setGsc = _set E.gsc
overGsc :: Over Int
overGsc = _over E.gsc

-- LENS name
-- _find l n = findScope $ S.lookup n . (^. l)
-- findScope f = getScopes >>= \ss -> return $ listToMaybe (M.mapMaybe f ss)

findPrim :: Find Macro
findPrim n = fmap (S.lookup n) (_get E.prims)

getScopes :: Lemonade [Scope]
getScopes = _get E.scopes
setScopes :: Set [Scope]   
setScopes = _set E.scopes
overScopes :: Over [Scope] -- Runs a function over the scopes
overScopes = _over E.scopes

-- Apols for the pun. Like overScopes, except permits returning an error
eitherScopes :: ([Scope] -> Either LemonError [Scope]) -> Lemonade ()
eitherScopes f = getScopes >>= \s -> case (f s) of
                   Right s2 -> setScopes s2
                   Left  e  -> throwE e

-- Creates a new scope on the stack
enscope :: Lemonade ()
enscope = overScopes (emptyScope:)

-- Removes the top scope if doing so wouldn't remove the last scope
unscope, descope :: Lemonade ()
unscope = eitherScopes h
  where h (_:t1:ts) = Right (t1:ts)
        h _     = Left StackEmpty
descope = unscope

scoped :: Lemonade a -> Lemonade a
scoped f = enscope >> f >>= \r -> descope >> return r

-- Gets the top scope on the stack
currentScope :: Lemonade Scope
currentScope = fmap head getScopes

-- Runs a function over the top scope on the stack
overCurrentScope :: (Scope -> Scope) -> Lemonade ()
overCurrentScope f = overScopes (& ix 0 %~ f)

findScope :: (Scope -> Maybe a) -> Lemonade (Maybe a)
findScope f = getScopes >>= \ss -> return $ listToMaybe (M.mapMaybe f ss)

-- _find :: LENS -> NAME -> Lemonade (Maybe a)
_find l n = findScope $ S.lookup n . (^. l)


findMacro :: Find Macro
findMacro = _find E.macros
findFun   :: Find Func
findFun   = _find E.funcs
findBind  :: Find Term
findBind  = _find E.binds


-- _put :: LENS -> NAME -> VAL -> Lemonade ()
_put l n v = overCurrentScope (& l %~ S.insert n v)

putMacro :: Put Macro
putMacro = _put E.macros
putFun   :: Put Func
putFun   = _put E.funcs
putBind  :: Put Term
putBind  = _put E.binds
