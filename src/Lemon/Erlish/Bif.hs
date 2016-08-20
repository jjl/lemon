{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings, ScopedTypeVariables #-}
module Lemon.Erlish.Bif where

import Control.Lens hiding (List)
import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Lemon.Erlish.Data as E
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map as M
import Prelude as P

type Fun1 = forall r v w. Term v w -> Eff r (Term v w)

type FunError1 = forall r v w. WithError r v w
    => Term v w -> Eff r (Term v w)
type FunError2 = forall r v w. WithError r v w
    => Term v w -> Term v w -> Eff r (Term v w)
type FunError3 = forall r v w. WithError r v w
    => Term v w -> Term v w -> Term v w -> Eff r (Term v w)

-- math

abs :: FunError1
abs (Int n w)   = return $ Int   (P.abs n) w
abs (Float n w) = return $ Float (P.abs n) w
abs f = throwError (InvalidArgument "abs/1 takes a number" f)

float :: FunError1
float (Int i w)       = return $ E.Float (fromIntegral i) w
float f@(E.Float _ _) = return f
float f = throwError (InvalidArgument "float/1 takes a number" f)
         
round :: FunError1
round i@(E.Int _ _) = return i
round (E.Float f w) = return $ E.Int (P.round f) w
round f = throwError (InvalidArgument "round/1 takes a number" f)

trunc :: FunError1
trunc i@(E.Int _ _) = return i
trunc (E.Float f w) = return $ E.Int (P.floor f) w
trunc f = throwError (InvalidArgument "trunc/1 takes a number" f)

-- TODO: type comparisons -- check invariants!
-- max(term1,term2) -> term
-- min(term1,term2) -> term

-- TODO: how are we representing functions? 
-- apply (fun,argslist)
-- apply (module, fun,argslist)

-- conversions:
-- TODO: what should we do?
-- atom_to_binary (atom, latin1|utf8 ) -> binary
-- atom_to_binary (E.Atom a) (E.Atom "latin1")
-- atom_to_binary (E.Atom a) (E.Atom "utf8") = throwError (InvalidArgument "atom_to_binary/2 not yet implemented" f)
-- atom_to_binary (E.Atom a) f = throwError (InvalidArgument "atom_to_binary/2" f)
-- atom_to_binary f _ = throwError (InvalidArgument "atom_to_binary/2 expects an atom in position 1" f)

-- binary_to_atom (binary, latin1|utf8) -> atom

-- collections:
erl_append_element :: FunError2
erl_append_element (E.Tuple t w) i = return $ E.Tuple (t ++ [i]) w
erl_append_element f _ = throwError (InvalidArgument "erlang:append_element/2 expects a tuple as first argument" f)

-- TODO: check my invariants
-- erl_delete_element:: E.Term -> E.Term -> Erlish
-- erl_delete_element (E.Int i) (E.Tuple t) = 
-- erl_delete_element (E.Int _) f = throwError (InvalidArgument "erlang:delete_element/2 expects a tuple as second argument" f)
-- erl_delete_element f _ = throwError (InvalidArgument "erlang:delete_element/2 expects an int as first argument" f)

length :: FunError1
length (E.List l w) = return $ E.Int (fromIntegral $ P.length l) w
length f = throwError (InvalidArgument "length/1 expects a list" f)

-- TODO: check my invariants!
-- element :: E.Term -> E.Term -> Erlish
-- element (E.Int i w) (E.Tuple t) = 
-- element (E.Int i _) f = throwError (InvalidArgument "element/2 expects a tuple as second argument" f)
-- element f _ = throwError (InvalidArgument "element/2 expects an int as first argument" f)

-- TODO: check my invariants!
hd :: FunError1
hd (E.List (h:_) _) = return h
hd f = throwError (InvalidArgument "hd/1 expects a non-empty list" f)

-- TODO: check my invariants!
-- erl_insert_element :: E.Term -> E.Term -> E.Term -> Erlish
-- erl_insert_element (E.Int idx) (T.Tuple t) term = ...
-- erl_insert_element (E.Int _) f _ = throwError (InvalidArgument "erlang:insert_element/3 expects a tuple in position 2" f)
-- erl_insert_element f _ _ = throwError (InvalidArgument "erlang:insert_element/3 expects an int in position 1" f)

list_to_tuple :: FunError1
list_to_tuple (E.List l w) = return (E.Tuple l w)
list_to_tuple f = throwError (InvalidArgument "list_to_tuple/1 expects a list" f)

-- TODO: check my invariants!
-- make_tuple2 :: E.Term -> E.Term -> Erlish
-- make_tuple2 (E.Int i) t | i > 0 = return (E.List $ L.replicate (fromIntegral i) t)
-- make_tuple2 f@(E.Int _) _ = throwError (InvalidArgument "make_tuple/2 expects a positive number" f)
-- make_tuple2 f _ = throwError (InvalidArgument "make_tuple/2 expects a number as first argument" f)
-- make_tuple(arity,defaultval,initlist) -> tuple

map_size :: FunError1
map_size (E.Map m w) = return $ E.Int (fromIntegral $ M.size m) w
map_size f = throwError (InvalidArgument "map_size/1 expects a map" f)

-- TODO: check my invariants
-- setelement(index,tuple,value) -> tuple
-- setelement (E.Int i) (E.Tuple t) v = ...
-- setelement (E.Int i) f _ = throwError (InvalidArgument "" f)
-- setelement f _ _ = throwError (InvalidArgument "" f)

-- TODO: check my invariants
-- size(tuple|binary) -> value
-- size (E.Tuple t) = 
-- size (E.Binary b) =
-- size f = throwError (InvalidArgument "size/1 expects a tuple or a binary" f)

-- TODO: check my invariants
-- tl(list) -> list (tail)
-- tl (E.List (h:t)) = ...
-- tl f = throwError (InvalidArgument "tl/1 expects a non-empty list" f)

tuple_size :: FunError1
tuple_size (E.Tuple t w) = return $ E.Int (fromIntegral $ P.length t) w
tuple_size f = throwError (InvalidArgument "tuple_size/1 expects a tuple" f)

tuple_to_list :: FunError1
tuple_to_list (E.Tuple t w) = return (E.List t w)
tuple_to_list f = throwError (InvalidArgument "tuple_to_list/1 expects a tuple" f)

-- Type tests
-- is_function -> bool

_at :: forall r v w. Text -> Term v w -> Eff r (Term v w)
_at n l = return $ Atom n (l ^. tCont)
is_atom :: Fun1
is_atom (Atom _ w) = return $ Atom "true" w
is_atom l          = _at "false" l

is_binary :: Fun1
is_binary b@(E.Binary _ _) = _at "true" b
is_binary b = _at "false" b

is_boolean :: Fun1
is_boolean b@(E.Atom a _) = _at h b
    where h = if a `elem` ["true","false"] then "true" else "false"
is_boolean b = _at "false" b

is_float :: Fun1
is_float f@(E.Float _ _) = _at "true" f
is_float f = _at "false" f

is_integer :: Fun1
is_integer i@(E.Int _ _) = _at "true" i
is_integer i = _at "false" i

is_list :: Fun1
is_list l@(E.List _ _) = _at "true" l
is_list l = _at "false" l

is_map :: Fun1
is_map m@(E.Map _ _) = _at "true" m
is_map m = _at "false" m

is_number :: Fun1
is_number n@(E.Int _ _)   = _at "true" n
is_number n@(E.Float _ _) = _at "true" n
is_number n = _at "false" n

is_tuple :: Fun1
is_tuple t@(E.Tuple _ _) = _at "true" t
is_tuple t = _at "false" t

-- -- Binaries

-- -- split_binary(bin,pos) -> tuple(binary,binary)

-- -- misc:
-- error :: E.Term -> Erlish
-- error (E.Binary b) = throwError (RuntimeError b)
-- error f = throwError (InvalidArgument "error/1 (of all things!) takes a binary" f)

-- not convinced:
-- binary_to_existing_atom (binary,encoding)
-- erlang:convert_time_unit
-- binary_to_term(binary) -> term
-- binary_to_term(binary,opts) -> term
-- binary_to_float (binary) -> float
-- binary_to_integer (binary) -> int
-- binary_to_integer (binary, base) -> int
-- binary_to_list (binary) -> string
-- atom_to_list (string) -> string
-- atom_to_list (string, encoding) -> string
-- integer_to_binary(int) -> binary
-- integer_to_binary(int,base) -> binary
-- integer_to_list(int,base) -> string
-- float_to_binary(float) -> binary
-- float_to_binary(float, options) -> binary
-- float_to_list(float) -> string
-- float_to_list(float, opts) -> string

