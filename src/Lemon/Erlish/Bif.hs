{-# LANGUAGE OverloadedStrings #-}
module Lemon.Erlish.Bif where

import Prelude as P
import Control.Monad.Trans.Except
import Lemon.Erlish.Monad
import Lemon.Erlish.Data as E
import qualified Data.List as L
import qualified Data.Map as M

-- math

abs :: E.Term -> Erlish
abs (E.Int i)   = return $ E.Int (P.abs i)
abs (E.Float f) = return $ E.Float (P.abs f)
abs f = throwE (InvalidArgument "abs/1 takes a number" f)

float :: E.Term -> Erlish
float (E.Int i)   = return $ E.Float $ fromIntegral i
float f@(E.Float _) = return f
float f = throwE (InvalidArgument "float/1 takes a number" f)
         
round :: E.Term -> Erlish
round i@(E.Int _) = return i
round (E.Float f) = return $ E.Int (P.round f)
round f = throwE (InvalidArgument "round/1 takes a number" f)

trunc :: E.Term -> Erlish
trunc i@(E.Int _) = return i
trunc (E.Float f) = return $ E.Int (P.floor f)
trunc f = throwE (InvalidArgument "trunc/1 takes a number" f)

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
-- atom_to_binary (E.Atom a) (E.Atom "utf8") = throwE (InvalidArgument "atom_to_binary/2 not yet implemented" f)
-- atom_to_binary (E.Atom a) f = throwE (InvalidArgument "atom_to_binary/2" f)
-- atom_to_binary f _ = throwE (InvalidArgument "atom_to_binary/2 expects an atom in position 1" f)

-- binary_to_atom (binary, latin1|utf8) -> atom

-- collections:
erl_append_element :: E.Term -> E.Term -> Erlish
erl_append_element (E.Tuple t) i = return $ E.Tuple $ t ++ [i]
erl_append_element f _ = throwE (InvalidArgument "erlang:append_element/2 expects a tuple as first argument" f)

-- TODO: check my invariants
-- erl_delete_element:: E.Term -> E.Term -> Erlish
-- erl_delete_element (E.Int i) (E.Tuple t) = 
-- erl_delete_element (E.Int _) f = throwE (InvalidArgument "erlang:delete_element/2 expects a tuple as second argument" f)
-- erl_delete_element f _ = throwE (InvalidArgument "erlang:delete_element/2 expects an int as first argument" f)

length :: E.Term -> Erlish
length (E.List l) = return $ E.Int $ fromIntegral (P.length l)
length f = throwE (InvalidArgument "length/1 expects a list" f)

-- TODO: check my invariants!
-- element :: E.Term -> E.Term -> Erlish
-- element (E.Int i) (E.Tuple t) = 
-- element (E.Int i) f = throwE (InvalidArgument "element/2 expects a tuple as second argument" f)
-- element f _ = throwE (InvalidArgument "element/2 expects an int as first argument" f)

-- TODO: check my invariants!
-- hd (E.List (h:_)) = return h
-- hd l@(E.List _) = throwE (InvalidArgument "hd/1 called with an empty list" l)
-- hd f = throwE (InvalidArgument "hd/1 expects a non-empty list" f)

-- TODO: check my invariants!
-- erl_insert_element :: E.Term -> E.Term -> E.Term -> Erlish
-- erl_insert_element (E.Int idx) (T.Tuple t) term = ...
-- erl_insert_element (E.Int _) f _ = throwE (InvalidArgument "erlang:insert_element/3 expects a tuple in position 2" f)
-- erl_insert_element f _ _ = throwE (InvalidArgument "erlang:insert_element/3 expects an int in position 1" f)

list_to_tuple :: E.Term -> Erlish
list_to_tuple (E.List l) = return (E.Tuple l)
list_to_tuple f = throwE (InvalidArgument "list_to_tuple/1 expects a list" f)

-- TODO: check my invariants!
make_tuple2 :: E.Term -> E.Term -> Erlish
make_tuple2 (E.Int i) t | i > 0 = return (E.List $ L.replicate (fromIntegral i) t)
make_tuple2 f@(E.Int _) _ = throwE (InvalidArgument "make_tuple/2 expects a positive number" f)
make_tuple2 f _ = throwE (InvalidArgument "make_tuple/2 expects a number as first argument" f)
-- make_tuple(arity,defaultval,initlist) -> tuple

map_size :: E.Term -> Erlish
map_size (E.Map m) = return $ E.Int (fromIntegral $ M.size m)
map_size f = throwE (InvalidArgument "map_size/1 expects a map" f)

-- TODO: check my invariants
-- setelement(index,tuple,value) -> tuple
-- setelement (E.Int i) (E.Tuple t) v = ...
-- setelement (E.Int i) f _ = throwE (InvalidArgument "" f)
-- setelement f _ _ = throwE (InvalidArgument "" f)

-- TODO: check my invariants
-- size(tuple|binary) -> value
-- size (E.Tuple t) = 
-- size (E.Binary b) =
-- size f = throwE (InvalidArgument "size/1 expects a tuple or a binary" f)

-- TODO: check my invariants
-- tl(list) -> list (tail)
-- tl (E.List (h:t)) = ...
-- tl f = throwE (InvalidArgument "tl/1 expects a non-empty list" f)

tuple_size :: E.Term -> Erlish
tuple_size (E.Tuple t) = return $ E.Int (fromIntegral $ P.length t)
tuple_size f = throwE (InvalidArgument "tuple_size/1 expects a tuple" f)

tuple_to_list :: E.Term -> Erlish
tuple_to_list (E.Tuple t) = return (E.List t)
tuple_to_list f = throwE (InvalidArgument "tuple_to_list/1 expects a tuple" f)

-- Type tests
-- is_function -> bool

_bool :: Bool -> Erlish
_bool = return . E.Bool 

is_atom :: E.Term -> Erlish
is_atom (E.Atom _) = _bool True
is_atom _ = _bool False

is_binary :: E.Term -> Erlish
is_binary (E.Binary _) = _bool True
is_binary _ = _bool False

is_boolean :: E.Term -> Erlish
is_boolean (E.Bool _) = _bool True
is_boolean _ = _bool False

is_float :: E.Term -> Erlish
is_float (E.Float _) = _bool True
is_float _ = _bool False

is_integer :: E.Term -> Erlish
is_integer (E.Int _) = _bool True
is_integer _ = _bool False

is_list :: E.Term -> Erlish
is_list (E.List _) = _bool True
is_list _ = _bool False

is_map :: E.Term -> Erlish
is_map (E.Map _) = _bool True
is_map _ = _bool False

is_number :: E.Term -> Erlish
is_number (E.Int _) = _bool True
is_number (E.Float f) = _bool True
is_number _ = _bool False

is_tuple :: E.Term -> Erlish
is_tuple (E.Tuple _) = _bool True
is_tuple _ = _bool False

-- Binaries

-- split_binary(bin,pos) -> tuple(binary,binary)

-- misc:
error :: E.Term -> Erlish
error (E.Binary b) = throwE (RuntimeError b)
error f = throwE (InvalidArgument "error/1 (of all things!) takes a binary" f)

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

