{-# LANGUAGE DataKinds, GADTs, KindSignatures,
    MultiParamTypeClasses, RecordWildCards, TemplateHaskell #-}
module Lemon.Erlish.Data where
  -- ( UserDefinable(..)
  -- , Error(..), Fun(..), Term(..), Macro(..), Module(..), 
  -- , Func(), Location(), Scope(), LemonState(), LemonError(..), Scope(..), Global(..)
  -- , Globals, Lexicals
  -- , funMeta, funFun, macMeta, macMac, modName, modMeta, modFuns, modMacs
  -- , scopeFuns, scopeMacs, scopeVals, builtins, mods, current
  -- ) where

import Control.Lens hiding (List)
import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.State
import Data.Map.Strict as M hiding (map, mapMaybe)
import Data.Maybe as Maybe
import Data.Scientific
import Data.Text (Text)
import Prelude as P hiding (map)

class UserDefinable a where
  isPrimitive   :: a -> Bool
  isUserDefined :: a -> Bool
  isPrimitive   = not . isUserDefined
  isUserDefined = not . isPrimitive

data Error form = InvalidArgument Text form
                | StackEmpty
                | RuntimeError Text

data Fun t v w = PrimFun { _funMeta :: Map t t
                         , _funFun  :: t -> Eff w t }
               | UserFun { _funMeta :: Map t t }
makeLenses ''Fun

instance UserDefinable (Fun t v w) where
  isPrimitive (PrimFun{..}) = True
  isPrimitive _ = False

type TermMap v w = Map (Term v w) (Term v w)
type ErlyFun v w = Fun (Term v w) v w

data Term v w = Int    { _tInt   :: Integer,     _tCont :: v }
              | Float  { _tFloat :: Scientific,  _tCont :: v }
              | Atom   { _tText  :: Text,        _tCont :: v }
              | Binary { _tText  :: Text,        _tCont :: v } -- TODO: bytestring me?
              | Tuple  { _tItems :: [Term v w],  _tCont :: v }
              | List   { _tItems :: [Term v w],  _tCont :: v }
              | Map    { _tMap   :: TermMap v w, _tCont :: v }
              | Fun    { _tFun   :: ErlyFun v w, _tCont :: v }
makeLenses ''Term

data Macro t v w = PrimMacro { _macMeta :: Map t t
                             , _macMac  :: t -> Eff w t }
                 | UserMacro { _macMeta :: Map t t }
makeLenses ''Macro

instance UserDefinable (Macro t v w) where
  isPrimitive (PrimMacro{..}) = True
  isPrimitive _ = False

data ModScope v w = ModScope { _msMacs :: Map Text       (Macro (Term v w) v w)
                             , _msFuns :: Map (Text,Int) (Fun (Term v w) v w) }
makeLenses ''ModScope

data Module v w = Module { _modName  :: Text
                         , _modMeta  :: Map Text       (Term v w)
                         , _modScope :: ModScope v w }
makeLenses ''Module

data Match v w = Match (Term v w) (Term v w)

data Scope v w = Scope { _scopeFuns :: Map (Text,Int) (Fun (Term v w) v w)
                       , _scopeMacs :: Map Text       (Macro (Term v w) v w)
                       , _scopeVals :: Map Text (Term v w)}
makeLenses ''Scope

data Global v w = Global { _builtins :: ModScope v w
                         , _mods     :: Map Text (Module v w)
                         , _current  :: Maybe Text }
makeLenses ''Global

type Globals  v w = State (Global v w) v
type Lexicals v w = State [Scope v w] v

lookupStack :: (Scope v w -> Maybe a) -> [Scope v w] -> Maybe a
lookupStack t = listToMaybe . mapMaybe t

lookupBuiltin :: (ModScope v w -> Maybe a) -> Global v w -> Maybe a
lookupBuiltin f g = f (g ^. builtins)

lookupModule :: Text -> Global v w -> Maybe (Module v w)
lookupModule n g = M.lookup n (g ^. mods)

sLookupFun :: (Text,Int) -> Scope v w -> Maybe (Fun (Term v w) v w)
sLookupFun k s = M.lookup k (s ^. scopeFuns)

sLookupMacro :: Text -> Scope v w -> Maybe (Macro (Term v w) v w)
sLookupMacro n s = M.lookup n (s ^. scopeMacs)

sLookupVal :: Text -> Scope v w -> Maybe (Term v w)
sLookupVal n s = M.lookup n (s ^. scopeVals)

mLookupFun :: (Text,Int) -> ModScope v w -> Maybe (Fun (Term v w) v w)
mLookupFun k s = M.lookup k (s ^. msFuns)

mLookupMacro :: Text -> ModScope v w -> Maybe (Macro (Term v w) v w)
mLookupMacro k s = M.lookup k (s ^. msMacs)

-- In Erlang, there is a global ordering of terms. So it is for our subset
scoreTerm :: Term v w -> Int
scoreTerm (Int _ _)    = 1
scoreTerm (Float _ _)  = 1
scoreTerm (Atom _ _)   = 2
scoreTerm (Fun _ _)    = 3
scoreTerm (Tuple _ _)  = 4
scoreTerm (Map _ _)    = 5
scoreTerm (List _ _)   = 6
scoreTerm (Binary _ _) = 7

-- Maps are a special case whereby ints score lower than floats
-- I Blame "Hello, " Robert
scoreTermForMap :: Term v w -> Int
scoreTermForMap (Int _ _) = 0
scoreTermForMap o = scoreTerm o

-- toFloat :: Term -> Double
-- toFloat (Int i)   = fromIntegral i
-- toFloat (Float f) = f
-- toFloat _ = error "Not a number!"

-- -- TODO: This is buggy, here is the proper description:
-- -- When comparing an integer to a float, the term with the lesser
-- -- precision is converted into the type of the other term,
-- -- unless the operator is one of =:= or =/=. A float is more
-- -- precise than an integer until all significant figures of the
-- -- float are to the left of the decimal point. This happens when
-- -- the float is larger/smaller than +/-9007199254740992.0. The
-- -- conversion strategy is changed depending on the size of the
-- -- float because otherwise comparison of large floats and
-- -- integers would lose their transitivity.
-- compareNums :: Term -> Term -> Ordering
-- compareNums l r = (toFloat l) `compare` (toFloat r)

-- -- lists are compared "item-by-item". nil < non-empty list
-- -- hashed out on irc what this means
-- compareLists :: [Term] -> [Term] -> Ordering
-- compareLists (h1:t1) (h2:t2) = if r == EQ then compareLists t1 t2 else r
--   where r = h1 `compare` h2
-- compareLists (_:_) [] = GT
-- compareLists [] (_:_) = LT
-- compareLists [] [] = EQ

-- -- tuples are first compared by size, else as for lists
-- compareTuples :: [Term] -> [Term] -> Ordering
-- compareTuples l r =
--     if ll < lr then LT
--     else if ll > lr then GT
--     else compareLists l r
--   where ll = length l
--         lr = length r

-- -- TODO: implement
-- -- atoms are compared as if lists of characters
-- compareAtoms :: Text -> Text -> Ordering
-- compareAtoms = error "compareAtoms not implemented yet"

-- -- TODO: not finished. Need to think about how to handle the map special ordering
-- -- case better.
-- -- Maps are ordered by size -- done
-- -- two maps with the same size are compared by keys -- done
-- -- in ascending term order and then by values in key order. -- buggy, uses Ord Ordering :/
-- -- In maps key order integers types are considered less than floats types. -- buggy, uses Ord Ordering :/
-- compareMaps :: Map Term Term -> Map Term Term -> Ordering
-- compareMaps l r = help (M.size l) (M.size r)
--   where help cl cr | cl < cr = LT
--         help cl cr | cl > cr = GT
--         help _  _  = if (ks /= EQ) then ks
--                      else if (vs /= EQ) then vs
--                      else error "compareMaps not finished yet"
--           where ks = compareLists (M.keys  l) (M.keys  r)
--                 vs = compareLists (M.elems l) (M.elems r)
        

-- -- Todo: implement: compare byte by byte
-- compareBinaries :: Text -> Text -> Ordering
-- compareBinaries = error "compareBinaries not implemented yet"

-- compareTerms :: Term -> Term -> Ordering
-- compareTerms left right =
--     if (sl < sr) then LT
--     else if (sl > sr) then GT
--     else case left of
--       Int _ -> left `compareNums` right
--       Float _ -> left `compareNums` right
--       Atom a -> a `compareAtoms` (right ^. atom)
--       Fun _ -> EQ -- TODO: get confirmation from robert
--       Tuple t -> t `compareTuples` (right ^. items)
--       Map m -> m `compareMaps` (right ^. map)
--       List l -> l `compareLists` (right ^. items)
--       Binary b -> b `compareBinaries` (right ^. bin)
--   where sl = scoreTerm left
--         sr = scoreTerm right

-- instance Ord Term where
--   compare = compareTerms

-- data Location = Location
--   { _line :: Int, _col :: Int } deriving (Eq, Show)



