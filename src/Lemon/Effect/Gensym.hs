{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Lemon.Effect.Gensym
  ( Gensym, gensym, runGensym
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Data.Text (Text, pack)

gensym :: Member Fresh r => Eff r Text
gensym = fmap h fresh
  where h = pack . ("gensym"++) . show

runGensym :: Eff (Fresh ': r) w -> Eff r w
runGensym = (flip runFresh') 0

type Gensym = Fresh
