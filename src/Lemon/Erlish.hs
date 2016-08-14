{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, KindSignatures, RankNTypes,
             ScopedTypeVariables, TypeOperators, ViewPatterns #-}
module Lemon.Erlish where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (List)
import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Monad.Freer.State as St
import Data.Maybe
import Data.Map.Strict as M hiding (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Lemon.AST as A
import Lemon.Effect.Stack as Stack
import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E
import Lemon.Erlish.Prim as Prim
import Prelude as P hiding (get, lookup)

-- termify :: Node -> Term v w
-- termify (Number n) = either E.Float E.Int (floatingOrInteger n)
-- termify (Char c)   = E.Int (fromIntegral $ fromEnum c)
-- termify (String s) = E.Binary s
-- termify (A.Atom s) = E.Atom s
-- termify (A.List l) = E.List (fmap termify l)

type Arr' r a = Arr r a a

orDo :: Monad m => m (Maybe a) -> Maybe a -> m (Maybe a)
orDo _ j@(Just _) = return j
orDo f Nothing = f

findCurrent :: Member (State (Global v w)) r => Eff r (Maybe (Module v w))
findCurrent = get >>= \g -> return $ case (g ^. current) of
  Just m -> (lookupModule m g)
  Nothing -> Nothing

findInGlobal :: Member (State (Global v w)) r => Arr r (ModScope v w -> Maybe a) (Maybe a)
findInGlobal f = fmap (lookupBuiltin f) get

findInStack :: Member (State [Scope  v w]) r => Arr r (Scope v w -> Maybe a) (Maybe a)
findInStack f = fmap (lookupStack f) get

findInModule :: Member (State (Global v w)) r => (Module v w -> Maybe a) -> Arr r Text (Maybe a)
findInModule f n = h `fmap` get
  where h g = maybe Nothing f (lookupModule n g)

findInCurrent :: Member (State (Global v w)) r => Arr r (ModScope v w -> Maybe a) (Maybe a)
findInCurrent f = h `fmap` findCurrent 
  where h = maybe Nothing (f . (^. modScope))

findAnywhere :: Members [State (Global v w), State [Scope v w]] r
                => (Scope v w -> Maybe a) -> Arr r (ModScope v w -> Maybe a) (Maybe a)
findAnywhere f f2 = findInStack f >>= orDo (findInCurrent f2) >>= orDo (findInGlobal f2)

findFun :: Members [State (Global v w), State [Scope  v w]] r
           => Arr r (Text,Int) (Maybe (Fun (Term v w) v w))
findFun na = findAnywhere (sLookupFun na) (mLookupFun na)

findMacro :: Members [State (Global v w), State [Scope  v w]] r
             => Arr r Text (Maybe (Macro (Term v w) v w))
findMacro n = findAnywhere (sLookupMacro n) (mLookupMacro n)

findMod :: Member (State (Global v w)) r => Arr r Text (Maybe (Module v w))
findMod   n = fmap (lookupModule n) get

findVal :: Member (State [Scope  v w]) r => Arr r Text (Maybe (Term v w))
findVal   n = fmap (lookupStack (sLookupVal n)) get

-- TODO: User macros
runMacro :: Monoid (Eff w (Term v w)) => Macro (Term v w) v w -> Arr' w (Term v w)
runMacro m ts | isPrimitive m = (m ^. macMac) ts
-- runMacro m ts = return 


-- Inco
macroexpand1 :: forall r v w. Arr r Text Bool
macroexpand1 s = m' >>= \m -> return $ case m of
    Just j  -> True
    Nothing -> False
  where m' :: forall r v1 w1. Eff r (Maybe (Macro (Term v1 w1) v1 w1))
        m' = findMacro s
-- macroexpand1 :: Members [State (Global v w), State [Scope  v w]] r => Arr' r (Term v w)
-- macroexpand1 t@(E.List ((E.Atom s w1):is) w) = findMacro s >>= expand
-- --  Just j -> runMacro j (E.List is w) -- WTF about the infinite type?
--   -- Nothing -> return t --return $ E.List  is w --case m of
--   where expand (Just m) = return t
--         expand _ = return t
-- macroexpand1 t = return t

-- macroexpand1 :: Term v w -> Eff (State (Global v w) ': State [Scope v w] ': r) (Term v w)
-- macroexpand1 t@(E.List ((E.Atom s _):is) v) = do m <- findMacro s
--                                                  return (E.List is v) -- 
-- macroexpand1 t = return t

-- macroexpand1 f@(List ((Atom s):_)) = findMacro s >>= maybe (return f) ($ f)
-- macroexpand1 :: E.Term -> Erlish
-- macroexpand1 f@(E.List ((E.Atom s):_)) = findMacro s >>= maybe (return f) ($ f)
-- macroexpand1 f = return f

-- macroexpand :: E.Term -> Erlish
-- macroexpand f = macroexpand1 f >>= \r -> if f == r then return f else macroexpand r

-- macroexpandAll :: E.Term -> Erlish
-- macroexpandAll f = macroexpand f >>= \f2 ->
--   case f2 of
--     (E.List (h:t)) -> scoped $
--       do h2 <- scoped $ macroexpandAll h
--          rs <- (macroexpandAll $ E.List t)
--          cons h2 rs
--     _ -> make f2

runErlish :: (Global v w) -> [Scope v w]
            -> Eff '[State [Scope v w], Fresh, State (Global v w)] v
            -> ((v, [Scope v w]), Global v w)
runErlish g ls = run . runGlobal g . runGensym 0 . runLexicals ls
  where runGlobal    = flip runState
        runGensym    = flip runFresh'
        runLexicals  = flip runState

--runAndTrace ls = runTrace . runErlish ls
-- eval :: E.Term -> Erlish
-- eval (E.List l) = ...
-- eval t = t -- Anything else evaluates to itself

