{-# LANGUAGE TemplateHaskell #-}
module Lemon.Erlish.Data
  ( Macro, Lemonade, Erlish
  , Func(), Location(), Scope(), LemonState(), LemonError(..)
  , args, body, meta, line, col, macros, funcs, binds, scopes, gsc, prims
  , Term(Int, Float, Fun, Atom, Binary, Tuple, List, Map)
  , int, float, fun, atom, bin, items, map
  , nil, emptyScope, emptyState
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Map.Strict as M hiding (map)
import Data.Text (Text)
import Prelude as P hiding (map)

data Func = Func { _args :: [Term]
                 , _body :: [Term]
                 , _meta :: Map Text Term
                 } deriving (Eq, Show)

data Term = Int    { _int   :: Integer }
          | Float  { _float :: Double }
          | Atom   { _atom  :: Text }
          | Fun    { _fun   :: Func }
          | Tuple  { _items :: [Term] }
          | List   { _items :: [Term] }
          | Binary { _bin   :: Text }
          | Map    { _map   :: (M.Map Term Term) }
            deriving (Eq, Show)

makeLenses ''Term

scoreTerm :: Term -> Int
scoreTerm (Int _)    = 1
scoreTerm (Float _)  = 1
scoreTerm (Atom _)   = 2
scoreTerm (Fun _)    = 3
scoreTerm (Tuple _)  = 4
scoreTerm (Map _)    = 5
scoreTerm (List _)   = 6
scoreTerm (Binary _) = 7

-- Maps score integers lower than floats for whatever reason
scoreTermForMap :: Term -> Int
scoreTermForMap (Int _) = 0
scoreTermForMap o = scoreTerm o

toFloat :: Term -> Double
toFloat (Int i)   = fromIntegral i
toFloat (Float f) = f
toFloat _ = error "Not a number!"

-- TODO: This is buggy, here is the proper description:
-- When comparing an integer to a float, the term with the lesser
-- precision is converted into the type of the other term,
-- unless the operator is one of =:= or =/=. A float is more
-- precise than an integer until all significant figures of the
-- float are to the left of the decimal point. This happens when
-- the float is larger/smaller than +/-9007199254740992.0. The
-- conversion strategy is changed depending on the size of the
-- float because otherwise comparison of large floats and
-- integers would lose their transitivity.
compareNums :: Term -> Term -> Ordering
compareNums l r = (toFloat l) `compare` (toFloat r)

-- lists are compared "item-by-item". nil < non-empty list
-- hashed out on irc what this means
compareLists :: [Term] -> [Term] -> Ordering
compareLists (h1:t1) (h2:t2) = if r == EQ then compareLists t1 t2 else r
  where r = h1 `compare` h2
compareLists (_:_) [] = GT
compareLists [] (_:_) = LT
compareLists [] [] = EQ

-- tuples are first compared by size, else as for lists
compareTuples :: [Term] -> [Term] -> Ordering
compareTuples l r =
    if ll < lr then LT
    else if ll > lr then GT
    else compareLists l r
  where ll = length l
        lr = length r

-- TODO: implement
-- atoms are compared as if lists of characters
compareAtoms :: Text -> Text -> Ordering
compareAtoms = error "compareAtoms not implemented yet"

-- TODO: not finished. Need to think about how to handle the map special ordering
-- case better.
-- Maps are ordered by size -- done
-- two maps with the same size are compared by keys -- done
-- in ascending term order and then by values in key order. -- buggy, uses Ord Ordering :/
-- In maps key order integers types are considered less than floats types. -- buggy, uses Ord Ordering :/
compareMaps :: Map Term Term -> Map Term Term -> Ordering
compareMaps l r = help (M.size l) (M.size r)
  where help cl cr | cl < cr = LT
        help cl cr | cl > cr = GT
        help _  _  = if (ks /= EQ) then ks
                     else if (vs /= EQ) then vs
                     else error "compareMaps not finished yet"
          where ks = compareLists (M.keys  l) (M.keys  r)
                vs = compareLists (M.elems l) (M.elems r)
        

-- Todo: implement: compare byte by byte
compareBinaries :: Text -> Text -> Ordering
compareBinaries = error "compareBinaries not implemented yet"

compareTerms :: Term -> Term -> Ordering
compareTerms left right =
    if (sl < sr) then LT
    else if (sl > sr) then GT
    else case left of
      Int _ -> left `compareNums` right
      Float _ -> left `compareNums` right
      Atom a -> a `compareAtoms` (right ^. atom)
      Fun _ -> EQ -- TODO: get confirmation from robert
      Tuple t -> t `compareTuples` (right ^. items)
      Map m -> m `compareMaps` (right ^. map)
      List l -> l `compareLists` (right ^. items)
      Binary b -> b `compareBinaries` (right ^. bin)
  where sl = scoreTerm left
        sr = scoreTerm right

instance Ord Term where
  compare = compareTerms

nil :: Term
nil = List []

data Location = Location
  { _line :: Int, _col :: Int } deriving (Eq, Show)

type Macro = Term -> Erlish

data Scope = Scope { _macros :: Map Text Macro
                   , _funcs  :: Map Text Func
                   , _binds  :: Map Text Term }

emptyScope :: Scope
emptyScope = Scope M.empty M.empty M.empty

data LemonState = LemonState { _scopes :: [Scope]
                             , _gsc :: Int
                             , _prims :: Map Text Macro }

emptyState :: LemonState
emptyState = LemonState [emptyScope] 0 M.empty

data LemonError = InvalidArgument Text Term
                | RuntimeError Text
                | StackEmpty
                
type Lemonade = ExceptT LemonError (State LemonState)
  
type Erlish = Lemonade Term

makeLenses ''Func
makeLenses ''Location
makeLenses ''Scope
makeLenses ''LemonState
