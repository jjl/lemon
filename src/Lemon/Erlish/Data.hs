{-# LANGUAGE TemplateHaskell #-}
module Lemon.Erlish.Data
  ( Macro, Lemonade, Erlish
  , Func(), Location(), Scope(), LemonState(), LemonError(..)
  , args, body, line, col, macros, funcs, binds, scopes, gsc, prims
  , Term(Int, Float, Fun, Atom, Binary, Tuple, List, Bool, Char, Map)
  , int, float, fun, atom, bin, items, bool, char, map
  , emptyScope, emptyState
  ) where

import Control.Lens
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Map.Strict as M hiding (map)
import Data.Text hiding (map)
import qualified Prelude as P

data Func = Func { _args :: [Term]
                 , _body :: [Term]
                 } deriving (P.Eq, P.Show)

data Term = Int    { _int   :: P.Integer }
          | Float  { _float :: P.Float }
          | Fun    { _fun   :: Func }
          | Atom   { _atom  :: Text }
          | Binary { _bin   :: Text }
          | Tuple  { _items :: [Term] }
          | List   { _items :: [Term] }
          | Bool   { _bool  :: P.Bool }
          | Char   { _char  :: P.Char }
          | Map    { _map   :: (M.Map Term Term) }
            deriving (P.Eq, P.Show)

data Location = Location
  { _line :: P.Int, _col :: P.Int } deriving (P.Eq, P.Show)

type Macro = Term -> Erlish

data Scope = Scope { _macros :: Map Text Macro
                   , _funcs  :: Map Text Func
                   , _binds  :: Map Text Term }

emptyScope :: Scope
emptyScope = Scope M.empty M.empty M.empty

data LemonState = LemonState { _scopes :: [Scope]
                             , _gsc :: P.Int
                             , _prims :: Map Text Macro }

emptyState :: LemonState
emptyState = LemonState [emptyScope] 0 M.empty

data LemonError = InvalidArgument Text Term
                | RuntimeError Text
                | StackEmpty
                
-- When life gives you lemons, make Lemonade
type Lemonade = ExceptT LemonError (State LemonState)

  
type Erlish = Lemonade Term

makeLenses ''Func
makeLenses ''Term
makeLenses ''Location
makeLenses ''Scope
makeLenses ''LemonState
