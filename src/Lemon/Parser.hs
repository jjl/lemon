{-# LANGUAGE OverloadedStrings #-}
module Lemon.Parser
  ( bareAtom, quotedAtom, bitstring, character, charstring, number, linecomment -- non-extensible
  , quote, qquote, splice, unquote, list, autofun, tuple, map, lemon            -- extensible
  , prepend, prepend1 -- utilities
  ) where

import Lemon.AST as A

import Control.Applicative
import Data.Text (Text,pack)
import Text.Trifecta hiding (token)
import Prelude hiding (map)

-- NOTES:
-- 1. Currently, this works by careful ordering
-- 2. Currently, we delegate quasiquoting hard work to macroexpansion
-- 3. We would like to make more of trifecta
-- 4. Some of these patterns match too much or too little and need fixing

type LemonParser = Parser Node
type Parsers = [(Text,LemonParser)]

-- Parsers that are not pluggable

lineend :: Parser ()
lineend = (newline >> return ()) <|> eof

-- TODO: This is a bit restrictive, what should we actually do?
bareAtom :: LemonParser
bareAtom = (fmap (A.Atom . pack) (some letter)) <?> "bareAtom"

-- TODO: This will take basically anything
quotedAtom :: LemonParser
quotedAtom = (fmap (A.Atom . pack) $ between (char '|') (char '|') (many anyChar)) <?> "quotedAtom"

number :: LemonParser
number = (fmap A.Number $ scientific) <?> "number"

-- TODO: should bits do the same escaping as string for literals?
bitstring :: LemonParser
bitstring = (fmap (A.String . pack) $ char '#' *> stringLiteral) <?> "bitstring"

character :: LemonParser
character = (fmap A.Char $ (char '\\' *> anyChar)) <?> "character"

-- TODO: Are haskell's string literals correct for us?
charstring :: LemonParser
charstring = (fmap (A.String . pack) stringLiteral) <?> "stringLiteral"

-- Parsers that are pluggable

prepend :: Text -> [Node] -> Node
prepend t n = A.List $ (A.Atom t):n
prepend1 :: Text -> Node -> Node
prepend1 t n = prepend t [n]

quote :: Parsers -> LemonParser
quote p = (fmap (prepend1 "q") $ char '\'' *> lemon p) <?> "quote"

_lQQ :: Parsers -> Parsers
_lQQ p = ("splice",splice p):("unquote",unquote p):p

qquote :: Parsers -> LemonParser
qquote p = (fmap (prepend1 "qq") $ char '`' *> (lemon $ _lQQ p)) <?> "qquote"

splice :: Parsers -> LemonParser
splice p = (fmap (prepend1 "splice") $ (string ",@") *> lemon (_lQQ p)) <?> "splice"

unquote :: Parsers -> LemonParser
unquote p = (fmap (prepend1 "unquote") $ char ',' *> lemon (_lQQ p)) <?> "unquote"

list :: Parsers -> LemonParser
list p = (fmap A.List $ parens (many $ lemon p)) <?> "list"

autofun :: Parsers -> LemonParser
autofun p = (fmap (prepend "autofn") (char '#' *> parens (some $ lemon p))) <?> "autofun"

tuple :: Parsers -> LemonParser
tuple p = (fmap (prepend "tuple") (brackets (many $ lemon p))) <?> "tuple"

map :: Parsers -> LemonParser
map p = (fmap (prepend "map") (braces (many $ lemon p))) <?> "map"

linecomment :: LemonParser
linecomment = (try $ fmap (prepend1 "comment" . A.String . pack) $ f) <?> "linecomment"
  where f = char ';' *> anyChar `manyTill` lineend <* lineend

lemon :: Parsers -> LemonParser
lemon ps = (whiteSpace *> choice (fmap snd (ps ++ predef))) <?> "lemon"
  where predef :: Parsers
        predef = [ ("linecomment", linecomment) -- ;
                 , ("bitstring",   bitstring)   -- #"
                 , ("autofun",     autofun ps)  -- #(
                 , ("q",           quote ps)    -- '
                 , ("qq",          qquote ps)   -- `
                 , ("quoted-atom", quotedAtom)  -- |
                 , ("list",        list ps)     -- (
                 , ("tuple",       tuple ps)    -- [
                 , ("map",         map ps)      -- {
                 , ("string",      charstring)  -- "
                 , ("character",   character)   -- \
                 , ("number",      number)     -- 
                 , ("bare-atom",   bareAtom)]   -- anything else
