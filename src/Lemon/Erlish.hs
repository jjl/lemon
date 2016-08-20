{-# language AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts #-}
{-# language OverloadedStrings, KindSignatures, RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications, TypeOperators, ViewPatterns #-}
module Lemon.Erlish where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Monad.Freer.State as St
import Data.Maybe
import Data.Map.Strict as M hiding (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Traversable
import Lemon.AST as A
import Lemon.Effect.Stack as Stack
import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E
import Lemon.Erlish.Prim as Prim
import Prelude as P

-- termify :: Arr r Node (Term v w)
-- termify (Number n) = return $ either E.Float E.Int (floatingOrInteger n)
-- termify (Char c)   = return $ E.Int (fromIntegral $ fromEnum c)
-- termify (String s) = return $ E.Binary s
-- termify (A.Atom s) = return $ E.Atom s
-- termify (A.List l) = return $ E.List (fmap termify l)

-- e.g. (return Nothing) `orDo` (return Just 1)
orDo :: Monad m => m (Maybe a) -> Maybe a -> m (Maybe a)
orDo _ j@(Just _) = return j
orDo f Nothing = f

-- e.g. findCurrent >>= \maybe_module
findCurrent :: WithGlobal r v w  => Eff r (Maybe (Module v w))
findCurrent = get >>= \g -> return $ case (g ^. current) of
  Just m -> (lookupModule m g)
  Nothing -> Nothing

-- e.g. findInGlobal (mLookupFun ("foo",1)) >>= \maybe_fun -> ...
findInGlobal :: WithGlobal r v w => Arr r (ModScope v w -> Maybe a) (Maybe a)
findInGlobal f = fmap (lookupBuiltin f) get

-- e.g. findInStack (sLookupFun ("foo",1)) >>= \maybe_fun -> ...
findInStack :: WithStack r v w => Arr r (Scope v w -> Maybe a) (Maybe a)
findInStack f = fmap (lookupStack f) get

-- e.g. findInModule (mLookupFun ("foo",1)) "othermodule" >>= \maybe_fun -> ...
findInModule :: WithGlobal r v w => (Module v w -> Maybe a) -> Arr r Text (Maybe a)
findInModule f n = h `fmap` get
  where h g = maybe Nothing f (lookupModule n g)

-- e.g. findInCurrent (sLookupFun ("foo",1)) >>= \maybe_fun -> ...
findInCurrent :: WithGlobal r v w => Arr r (ModScope v w -> Maybe a) (Maybe a)
findInCurrent f = h `fmap` findCurrent 
  where h = maybe Nothing (f . (^. modScope))

-- e.g. see findFun and findMacro
findAnywhere :: WithGS r v w => (Scope v w -> Maybe a) -> Arr r (ModScope v w -> Maybe a) (Maybe a)
findAnywhere f f2 = findInStack f >>= orDo (findInCurrent f2) >>= orDo (findInGlobal f2)

-- e.g. findFun ("foo",1) >>= \maybe_fun -> ...
findFun :: forall r v w. WithGS r v w => Arr r (Text,Int) (Maybe (Fun (Term v w) v w))
findFun na = findAnywhere (sLookupFun na) (mLookupFun @v @w na)

-- e.g. findMacro "foo" >>= \maybe_macro -> ...
findMacro :: forall r v w. WithGS r v w => Arr r Text (Maybe (Macro (Term v w) w))
findMacro n = findAnywhere (sLookupMacro n) (mLookupMacro @v @w n)

-- e.g. findModule "mymod" >>= \maybe_mod -> ...
findMod :: WithGlobal r v w => Arr r Text (Maybe (Module v w))
findMod n = fmap (lookupModule n) get

-- e.g. findVal "myval" >>= \maybe_term -> ...
findVal :: WithStack r v w => Arr r Text (Maybe (Term v w))
findVal n = fmap (lookupStack (sLookupVal n)) get

runMacro :: Macro (Term v w) w -> Arr' w (Term v w)
runMacro m = (m ^. macMac)

enscope :: forall v w. WithStack w v w => Eff w ()
enscope = Stack.push (Scope @v @w M.empty M.empty M.empty)
descope :: forall v w. WithES w v w => Eff w (Scope v w)
descope = Stack.pop @w @(Scope v w) @(Term v w)

scoped :: forall v w. WithES w v w => Eff w (Term v w) -> Eff w (Term v w)
scoped c = do _ <- enscope @v @w
              r <- c
              _ <- descope @v @w
              return r

macroexpand1 :: forall v w. WithGS w v w => (Text -> Bool) -> Arr' w (Term v w)
macroexpand1 p t@(E.List ((E.Atom s _):is) w) =
  if not (p s) then return t
  else findMacro @w @v @w s >>= expand
  where expand :: Arr w (Maybe (Macro (Term v w) w)) (Term v w)
        expand (Just m) = (m ^. macMac) (E.List is w)
        expand _ = return t
macroexpand1 _ t = return t

macroexpand :: forall v w. (Monoidal v w, WithGS w v w) => (Text -> Bool) -> Arr' w (Term v w)
macroexpand p t = macroexpand1 p t >>= h
  where h r | t == r = return r
        h r = macroexpand p r

-- firstly, macroexpands the given thing. then macroexpands everything inside it
macroexpandAll :: forall v w. (Monoidal v w, WithEGS w v w) => (Text -> Bool) -> Arr w (Term v w) (Term v w)
macroexpandAll p t = macroexpand p t >>= help
  where help (E.List (hd:ts) w) =
          do ts' <- mapM (\t' -> scoped (macroexpandAll p t')) ts
             return (E.List (hd:ts') w)
        help (E.Tuple ts w) =
          do ts' <- mapM (\t' -> scoped (macroexpandAll p t')) ts
             return (E.Tuple ts' w)
        help t' = return t'

eval :: forall v w. (Monoidal v w, WithEGS w v w) => Arr w (Term v w) (Term v w)
eval (E.List ((E.Fun f _):t) w) = mapM eval t >>= \ts -> (f ^. funFun) (E.List ts w)
eval (E.List (a2@(E.Atom a w2):t) w) = findFun @w @v @w (a,length t) >>= maybe err nxt
  where err = (throwError (InvalidArgument "Tried to call nonexistent function" a2))
        nxt f = eval (E.List ((E.Fun f w2):t) w)
eval t = return t

-- Runs a list of top-level terms in macroexpand mode
runMacroexpander :: (Global v w) -> [Scope v w] -> [Term v w] -> Global v w
runMacroexpander g ls es = P.foldr runOne g es
   where runOne t g' = snd $ (run . runGlobal g' . runGensym 0 . runLexicals ls . return) t
         runGlobal    = flip runState
         runGensym    = flip runFresh'
         runLexicals  = flip runState

runInterpreter :: (Global v w) -> [Scope v w] -> [Term v w] -> Global v w
runInterpreter g ls es = P.foldr runOne g es
   where runOne t g' = snd $ (run . runGlobal g' . runLexicals ls . return) t
         runGlobal    = flip runState
         runLexicals  = flip runState
