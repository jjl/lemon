module Lemon.AST (Node(..)) where

import Data.Scientific (Scientific)
import Data.Text (Text)

import qualified Prelude as P

data Node = Number Scientific
          | Char P.Char
          | String Text
          | Atom Text
          | List [Node]
            deriving (P.Eq, P.Show)

