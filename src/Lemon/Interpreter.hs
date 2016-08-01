{-# LANGUAGE OverloadedStrings #-}
module Lemon.Interpreter where

import Lemon.AST
import Data.Map.Strict as M
import Data.Text (Text)

-- type Macros = Map Text (ASTNode -> ASTNode)

-- macroexpand1 :: Macros -> ASTNode -> ASTNode
-- macroexpand1 m l@(LList ((LAtom k):_)) = maybe l ($ l) (M.lookup k m)
-- macroexpand1 _ a = a

-- macroexpand :: Macros -> ASTNode -> ASTNode
-- macroexpand m l = if r == l then r else macroexpand m r
--   where r = macroexpand1 m l

-- macroexpandAll :: Macros -> ASTNode -> ASTNode
-- macroexpandAll m l@(LList l2) = fmap macroexpandAll r

--     where r = macroexpand l

