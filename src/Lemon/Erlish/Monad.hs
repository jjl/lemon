{-# LANGUAGE FlexibleInstances, OverloadedStrings, TemplateHaskell, TypeSynonymInstances #-}
module Lemon.Erlish.Monad where
  -- ( Set, Over, Put -- type aliases
  -- , make
  -- , getGsc, setGsc, overGsc
  -- , getScopes, setScopes, overScopes, eitherScopes
  -- , enscope, descope, unscope, scoped
  -- , currentScope, overCurrentScope
  -- , findPrim, findScope, findMacro, findFun, findBind
  -- , putMacro, putFun, putBind
  -- ) where

import Control.Lens hiding (Over)
import Control.Monad.Freer.Exception -- Early termination
import Control.Monad.Freer.Fresh     -- Lexical Scope
import Control.Monad.Freer.State     -- Globals

import Data.Map.Strict as S
import Data.Maybe as M
import Data.Text (Text)
import Lemon.Erlish.Data as E

-- -- LENS name
-- -- _find l n = findScope $ S.lookup n . (^. l)
-- -- findScope f = getScopes >>= \ss -> return $ listToMaybe (M.mapMaybe f ss)

-- findPrim :: Find Macro
-- findPrim n = fmap (S.lookup n) (_get E.prims)

-- getScopes :: Lemonade [Scope]
-- getScopes = _get E.scopes
-- setScopes :: Set [Scope]   
-- setScopes = _set E.scopes
-- overScopes :: Over [Scope] -- Runs a function over the scopes
-- overScopes = _over E.scopes

-- -- Apols for the pun. Like overScopes, except permits returning an error
-- eitherScopes :: ([Scope] -> Either LemonError [Scope]) -> Lemonade ()
-- eitherScopes f = getScopes >>= \s -> case (f s) of
--                    Right s2 -> setScopes s2
--                    Left  e  -> throwE e

-- -- Gets the top scope on the stack
-- currentScope :: Lemonade Scope
-- currentScope = fmap head getScopes

-- -- Runs a function over the top scope on the stack
-- overCurrentScope :: (Scope -> Scope) -> Lemonade ()
-- overCurrentScope f = overScopes (& ix 0 %~ f)

-- findScope :: (Scope -> Maybe a) -> Lemonade (Maybe a)
-- findScope f = getScopes >>= \ss -> return $ listToMaybe (M.mapMaybe f ss)

-- -- _find :: LENS -> NAME -> Lemonade (Maybe a)
-- _find l n = findScope $ S.lookup n . (^. l)


-- findMacro :: Find Macro
-- findMacro = _find E.macros
-- findFun   :: Find Func
-- findFun   = _find E.funcs
-- findBind  :: Find Term
-- findBind  = _find E.binds


-- -- _put :: LENS -> NAME -> VAL -> Lemonade ()
-- _put l n v = overCurrentScope (& l %~ S.insert n v)

-- putMacro :: Put Macro
-- putMacro = _put E.macros
-- putFun   :: Put Func
-- putFun   = _put E.funcs
-- putBind  :: Put Term
-- putBind  = _put E.binds
