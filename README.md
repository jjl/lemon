# Lemon

Lisp on the Erlang Machine, Only Nicer

## Status: Pre-alpha

We can't generate code or do several other important things yet, but considering the amount of time that's gone in, I'm happy :)

Parser: Compiles, definitely buggy
Bifs: Mostly implemented, some need checking
Core modules: TODO

If you see a TODO, please fix it!

## Example

```lemon
(defmodule foo.bar
  (export (abc 0 2) (def 2)))

(defns abc
  {} ; metadata, optional
  "a + b = c" ; docstring, optional
  []          ; 0-arity params
  (abc 1 2)   ; 0-arity code. use 'do' for multiple forms
  [a b]       ; 2-arity params
  (+ a b))    ; 2-arity code. use 'do' for multiple forms

(defn def
  {}          ; metadata, optional
  "d - e = f" ; docstring, optional
  [a b]       ; params
  (- a b))    ; accompanying code

(defmodule foo.baz ; a second module in one file
  (import (as foo.bar b (as def ghi 2))))

(defn bar []
  (ghi 2 3))
```

## Overview

The project consists of the following parts:

* A parser for the lemon language
* The 'Erlish' VM, an interpreter used to expand macros
* Macros and primitives used by lemon on Erlish

## Notes

What core erlang modules should we implement?

candidates:
*  lists
*  string

We need to revisit which BIFs etc. to support in light of the fact that we can't avoid a bit more binary smarts.

The AST probably needs some tweaks

We need to translate Erlish back to lemon ASTs

We're pretty sure Text isn't the right type for bitstrings, but we'll fix it later
