{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts,
             RankNTypes, TypeOperators, ViewPatterns #-}
module Lemon.Erlish where

import Control.Applicative
import Control.Lens hiding (List)
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Monad.Freer.State as St
import Data.Maybe
import Data.Map.Strict as M hiding (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Lemon.AST as A
import Lemon.Effect.Gensym
import Lemon.Effect.Stack as Stack
import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E
import Lemon.Erlish.Prim as Prim
import Prelude as P hiding (get)

-- termify :: Node -> Term v w
-- termify (Number n) = either E.Float E.Int (floatingOrInteger n)
-- termify (Char c)   = E.Int (fromIntegral $ fromEnum c)
-- termify (String s) = E.Binary s
-- termify (A.Atom s) = E.Atom s
-- termify (A.List l) = E.List (fmap termify l)

-- getGlobal :: Lens' (Global t v w) a -> Eff '[State (Global t v w)] a
-- getGlobal l = get >>= return . (view l)
-- eitherGlobal l f = get >>= \g -> either id throwError (g ^. l)

-- withGlobal :: (Global t v w -> 

findLexical :: Member (State [Scope v w]) r =>
               (Scope v w -> Maybe a) -> Eff r (Maybe a)
findLexical t = fmap (listToMaybe . mapMaybe t) get

onGlobalScope :: (Scope v w -> a) -> Eff '[State (Global v w)] a
onGlobalScope f = fmap (f . (^. builtins)) get

onModule :: Text -> (Maybe (Module v w) -> a) -> Eff '[State (Global v w)] a
onModule n f = fmap (\g -> f $ M.lookup n (g ^. mods)) get

-- getCurrentModule :: Eff '[State (Global v w)] (Maybe Text)
-- getCurrentModule = fmap (^. current) get

-- onCurrentModule :: (Maybe (Module v w) -> a) -> Eff '[State (Global v w)] a
-- onCurrentModule f = get >>= \g -> case (g ^. current) of
--   Just j -> onModule j f
--   Nothing -> return (f Nothing)
-- onCurrentModule :: (Maybe (Module v w) -> a) -> Eff '[State (Global v w)] a
-- onCurrentModule f = get >>= \g -> case g ^. current of
--   Just j -> onModule j f
--   Nothing -> return $ f Nothing

lookupFun :: Text -> Int -> Scope v w -> Maybe (Fun (Term v w) v w)
lookupFun n a s = M.lookup (n,a) (s ^. scopeFuns)

lookupMac :: Text -> Scope v w -> Maybe (Macro (Term v w) v w)
lookupMac n s = M.lookup n (s ^. scopeMacs)

lookupVal :: Text -> Scope v w -> Maybe (Term v w)
lookupVal n s = M.lookup n (s ^. scopeVals)

orDo :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
orDo j@(Just _) _ = return j
orDo Nothing f = f

-- The types on these do not make sense to me yet but they return Eff r (Maybe a)
findHelper f _f2 = (findLexical f) >>= help2
  where -- help g = g `orDo` onModule f2
        help2 g = g `orDo` onGlobalScope f
findFun n a = findHelper (lookupFun n a) (maybe Nothing help)
  where help m = M.lookup (n,a) (m ^. modFuns)
findMacro n = findHelper (lookupMac n) id
findMod   n = fmap (M.lookup n . (^. mods)) get
findVal   n = findLexical (lookupVal n)

-- runPrimitiveMacro :: Macro (Term v w) v w -> [Term v w] -> Eff w (Term v w)
-- runPrimitiveMacro m ts = m2 (E.List ts)
--   where m2 :: (Term v w) -> Eff w (Term v w)
--         m2 = view macMac m

-- runMacro :: Macro (Term v w) v w -> [Term v w] -> Eff w (Term v w)
-- runMacro m ts | isPrimitive m = (m ^. macMac) (E.List ts)
-- runMacro m ts = return 

-- macroexpand1 t@(E.List ((E.Atom s):is)) =
--   findMacro s >>= \m -> case m of
--     Just m ->

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

-- runErlish :: Prims v w ->
--              Global (Term v w) w ->
--              [Lexical (Term v w) v] ->
--              Eff '[ State [Lexical (Term v w) v]
--                   , Gensym
--                   , Reader (Global (Term v w) w)] v ->
--              Eff r (v,[Lexical (Term v w) v])
runErlish g ls = run . runState g . runGensym . runStack ls

--runAndTrace ls = runTrace . runErlish ls
-- eval :: E.Term -> Erlish
-- eval (E.List l) = ...
-- eval t = t -- Anything else evaluates to itself

