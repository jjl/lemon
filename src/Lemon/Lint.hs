{-# LANGUAGE OverloadedStrings #-}
module Lemon.Lint where

import Lemon.AST
import Data.Text (Text)
import Language.CoreErlang.Syntax

data Error = Other Text

lintForms :: [Node] -> Either [Error] [Module]
lintForms _ = Left []
