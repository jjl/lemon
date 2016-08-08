{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, TypeOperators #-}
module Lemon.Effect.Stack
  ( push, peek, pop
  , runStack
  ) where

import Lemon.Erlish.Data
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Monad.Freer.State as St

push :: Member (State [s]) r => Arr r s ()
push i = St.modify (i:)

peek ::  Member (State [s]) r => Eff r s
peek = get >>= \st -> case st of
  h1:_ -> return h1

pop :: (Member (State [s]) r, Member (Exc (Error t)) r) => Eff r s
pop = get >>= \st -> case st of
  h1:t@(_:_) -> put t >> return h1
  -- h:t -> return $ Exc StackEmpty

-- dup :: Member (St.State [s]) r => Eff r ()
-- dup = peek >>= push

-- runStack :: [s] -> Eff (St.State [s] : r) w -> Eff r (w,[s])

runStack = St.runState
