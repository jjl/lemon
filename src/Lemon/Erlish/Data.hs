{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, FlexibleInstances,
             FlexibleContexts, KindSignatures, MultiParamTypeClasses, RankNTypes,
             RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}
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
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.State
import Data.Map.Strict as M hiding (map, mapMaybe)
import Data.Maybe as Maybe
import Data.Scientific
import qualified Data.Set as Set
import Data.Text (Text)
import Prelude as P hiding (map)

class UserDefinable a where
  isPrimitive   :: a -> Bool
  isUserDefined :: a -> Bool
  isPrimitive   = not . isUserDefined
  isUserDefined = not . isPrimitive

data Error form = InvalidArgument Text form
                | Unimplemented Text
                | StackEmpty
                | RuntimeError Text

data Fun t v w = PrimFun   { _funFun  :: t -> Eff w t }
               | Defn      { _funFun  :: t -> Eff w t, _funSource :: [t] }
               | Defns     { _funFun  :: t -> Eff w t, _funSource :: [t] }
data Macro t w = PrimMacro { _macMac  :: Arr w t t }
               | Defmacro  { _macMac  :: Arr w t t,    _macSource :: [t] }
               | Defmacros { _macMac  :: Arr w t t,    _macSource :: [t] }
makeLenses ''Fun
makeLenses ''Macro

instance UserDefinable (Fun t v w) where
  isPrimitive (PrimFun{..}) = True
  isPrimitive _ = False

type TermMap v w = Map (Term v w) (Term v w)
type ErlyFun v w = Fun (Term v w) v w
type ErlyMac v w = Macro (Term v w) w

data Term v w = Int    { _tInt   :: Integer,     _tCont :: v }
              | Float  { _tFloat :: Scientific,  _tCont :: v }
              | Atom   { _tText  :: Text,        _tCont :: v }
              | Binary { _tText  :: Text,        _tCont :: v } -- TODO: bytestring me?
              | Tuple  { _tItems :: [Term v w],  _tCont :: v }
              | List   { _tItems :: [Term v w],  _tCont :: v }
              | Map    { _tMap   :: TermMap v w, _tCont :: v }
              | Fun    { _tFun   :: ErlyFun v w, _tCont :: v }
makeLenses ''Term
instance Eq (Term v w) where
    (==) (Int v1 _)    (Int v2 _)    = v1 == v2
    (==) (Float v1 _)  (Float v2 _)  = v1 == v2
    (==) (Atom v1 _)   (Atom v2 _)   = v1 == v2
    (==) (Binary v1 _) (Binary v2 _) = v1 == v2
    (==) (Tuple v1 _)  (Tuple v2 _)  = v1 == v2
    (==) (List v1 _)   (List v2 _)   = v1 == v2
    (==) (Map v1 _)    (Map v2 _)    = v1 == v2
    -- (==) (Fun v1 _) (Fun v2 _) = v1 == v2
instance UserDefinable (Macro t v) where
  isPrimitive (PrimMacro{..}) = True
  isPrimitive _ = False

type Metadata v w = Map (Term v w) (Term v w)

data ModScope v w = ModScope { _msMacs :: Map Text       (Macro (Term v w) w, Metadata v w)
                             , _msFuns :: Map (Text,Int) (Fun (Term v w) v w, Metadata v w) }
makeLenses ''ModScope

data FunExp    = WildcardFE  | AritiesFE { _funExpArities  :: Set.Set Int }
data FunsExp   = WildcardFsE | NamesFsE  { _funsExpNames   :: Map Text FunExp }
data MacrosExp = WildcardMsE | NamesMsE  { _macrosExpNames :: Set.Set Text }
data ModExps = ModExps { _modExportsFuns :: Map Text FunExp, _modExportsMacros :: MacrosExp }

makeLenses ''FunExp
makeLenses ''FunsExp
makeLenses ''MacrosExp
makeLenses ''ModExps

instance Monoid FunExp where
  mempty = AritiesFE Set.empty
  mappend WildcardFE _ = WildcardFE
  mappend _ WildcardFE = WildcardFE
  mappend (AritiesFE a) (AritiesFE b) = AritiesFE (mappend a b)

instance Monoid FunsExp where
  mempty = NamesFsE M.empty
  mappend WildcardFsE _ = WildcardFsE
  mappend _ WildcardFsE = WildcardFsE
  mappend (NamesFsE a) (NamesFsE b) = NamesFsE (mappend a b)

instance Monoid MacrosExp where
  mempty = NamesMsE Set.empty
  mappend WildcardMsE _ = WildcardMsE
  mappend _ WildcardMsE = WildcardMsE
  mappend (NamesMsE a) (NamesMsE b) = NamesMsE (mappend a b)




data ModMeta v w = ModMeta { _modMetaMod  :: Metadata v w
                           , _modMetaFuns :: Map (Text,Int) (Metadata v w)
                           , _modMetaMacs :: Map Text (Metadata v w) }

data Module v w = Module { _modName    :: Text
                         , _modMeta    :: ModMeta v w
                         , _modExports :: ModExps
                         , _modScope   :: ModScope v w }
makeLenses ''Module

data Match v w = Match (Term v w) (Term v w)

data Scope v w = Scope { _scopeFuns :: Map (Text,Int) (Fun (Term v w) v w)
                       , _scopeMacs :: Map Text       (Macro (Term v w) w)
                       , _scopeVals :: Map Text (Term v w)}
makeLenses ''Scope

data Global v w = Global { _builtins :: ModScope v w
                         , _mods     :: Map Text (Module v w)
                         , _current  :: Maybe Text }
makeLenses ''Global

type Globals  v w = State (Global v w) v
type Lexicals v w = State [Scope v w] v

type WithError  r v w = Member (Exc (Error (Term v w))) r
type WithGlobal r v w = Member (State (Global v w)) r
type WithStack  r v w = Member (State [Scope v w]) r
type WithFresh  r     = Member Fresh r
type WithEG     r v w = Members [Exc (Error (Term v w)), State (Global v w)] r
type WithES     r v w = Members [Exc (Error (Term v w)), State [Scope v w]] r
type WithEGS    r v w = Members [Exc (Error (Term v w)), State (Global v w), State [Scope v w]] r
type WithGS     r v w = Members [State (Global v w), State [Scope v w]] r
type WithAll    r v w = Members [Exc (Error (Term v w)), State (Global v w), State [Scope v w], Fresh] r

type Monoidal v w = Monoid (Eff w (Term v w))

type Arr' r a = Arr r a a

lookupStack :: (Scope v w -> Maybe a) -> [Scope v w] -> Maybe a
lookupStack t = listToMaybe . mapMaybe t

lookupBuiltin :: (ModScope v w -> Maybe a) -> Global v w -> Maybe a
lookupBuiltin f g = f (g ^. builtins)

lookupModule :: Text -> Global v w -> Maybe (Module v w)
lookupModule n g = M.lookup n (g ^. mods)

sLookupFun :: (Text,Int) -> Scope v w -> Maybe (Fun (Term v w) v w)
sLookupFun k s = M.lookup k (s ^. scopeFuns)

sLookupMacro :: Text -> Scope v w -> Maybe (Macro (Term v w) w)
sLookupMacro n s = M.lookup n (s ^. scopeMacs)

sLookupVal :: Text -> Scope v w -> Maybe (Term v w)
sLookupVal n s = M.lookup n (s ^. scopeVals)

mLookupFunAndMeta :: forall v w. (Text,Int) -> ModScope v w -> Maybe (Fun (Term v w) v w, Metadata v w)
mLookupFunAndMeta k s = M.lookup k (s ^. msFuns)

mLookupFun :: forall v w. (Text,Int) -> ModScope v w -> Maybe (Fun (Term v w) v w)
mLookupFun na s = fmap fst (mLookupFunAndMeta na s)

mLookupFunMeta :: forall v w. (Text,Int) -> ModScope v w -> Maybe (Metadata v w)
mLookupFunMeta na s = fmap snd (mLookupFunAndMeta na s)

mLookupMacroAndMeta :: forall v w. Text -> ModScope v w -> Maybe (Macro (Term v w) w, Metadata v w)
mLookupMacroAndMeta k s = M.lookup k (s ^. msMacs)

mLookupMacro :: Text -> ModScope v w -> Maybe (Macro (Term v w) w)
mLookupMacro k s = fmap fst (mLookupMacroAndMeta k s)

mLookupMacroMeta :: forall v w. Text -> ModScope v w -> Maybe (Metadata v w)
mLookupMacroMeta n s = fmap snd (mLookupMacroAndMeta n s)

-- In Erlang, there is a global ordering of terms. So it is for our subset
scoreTermLax :: Term v w -> Int
scoreTermLax (Int _ _)    = 1
scoreTermLax (Float _ _)  = 1
scoreTermLax (Atom _ _)   = 2
scoreTermLax (Fun _ _)    = 3
scoreTermLax (Tuple _ _)  = 4
scoreTermLax (Map _ _)    = 5
scoreTermLax (List _ _)   = 6
scoreTermLax (Binary _ _) = 7

-- Maps are a special case whereby ints score lower than floats
-- I Blame "Hello, " Robert
-- This does however mean we have a unique integer for each type
scoreTermStrong :: Term v w -> Int
scoreTermStrong (Int _ _) = 0
scoreTermStrong o = scoreTermLax o

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



