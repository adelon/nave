{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}


{-|
This module defines lots of keywords and various filler 
phrases. The prefix underscore indicates that we do not 
care about the parse result (analogous to  discarding 
like @..; _ <- action; ...@ in do-notation). Moreover,  
this convention allows the use of short names that would 
otherwise be Haskell keywords or clash with other definitions. 
Care should be taken with introducing too many variants of
a keyword, lest the grammar becomes needlessly ambiguous!

The names are chosen using the following criteria:

   * As short as possible (e.g.: @_since@ over @_because@).

   * Sound like a keyword (e.g.: @_show@).

This module also defines symbols that have special uses 
(such as @_colon@ for its use in type signatures).

Since we do not care about the result type, we omit type
signatures in this module. Definitions are suppose to be
in alphabetical order. The explicit export list serves to
hide the ad-hoc shortening of @word@ to @w@.
-}
module Grammar.Keywords
   ( arity
   , _all
   , _an
   , _and
   , _are
   , _be
   , _can
   , _denote
   , _does
   , _equipped
   , _every
   , _exist
   , _exists
   , _extends
   , _fix
   , _for
   , _have
   , _if
   , _iff
   , _is
   , _itIsWrong
   , _let
   , _no
   , _not
   , _on
   , _oneOf
   , _or
   , _relation
   , _satisfying
   , _setOf
   , _show
   , _since
   , _some
   , _suchThat
   , _suppose
   , _that
   , _the
   , _then
   , _throughout
   , _thus
   , _unique
   , _write
   -- Symbols
   , _colon
   , _comma
   , _commaAnd
   , _commaOr
   , _defeq
   , _dot
   , _eq
   , _in
   ) where


import Base
import Grammar.Abstract (Tok(..))

import Text.Earley (Prod, (<?>), token)

import qualified Data.Text as Text


w lit = token (Word lit) <?> Text.unpack lit

arity :: Prod r String Tok Integer
arity = [1 | w "unary"] <|> [2 | w "binary"] <|> [3 | w "ternary"] <|> [4 | w "quaternary"] <|> [5 | w "quinary"]
   <|> [6 | w "senary"] <|> [7 | w "septenary"] <|> [8 | w "octonary"] <|> [9 | w "nonary"] <|> [10 | w "denary"]

_all = optional (w "for") *> w "all"
_an = w "a" <|> w "an"
_and = w "and"
_are = w "are"
_be = w "be"
_can = w "can"
_denote = w "denote"
_does = w "does"
_equipped = optional (w "equipped" <|> w "together") *>  "with"
_every = optional (w "for") *> w "every"
_exist = w "there" *> w "exist"
_exists = w "there" *> w "exists"
_extends = (_is *> _an) <|> (w "consists" *> w "of" *> _an)
_fix = w "fix"
_for = w "for"
_have = w "we" *> w "have" <* optional (w "that")
_if = w "if"
_iff = w "iff" <|> (w "if" *> w "and" *> w "only" *> w "if" *> pure "iff")
_is = w "is"
_itIsWrong = w "it" *> w "is" *> (w "not" *> w "the" *> w "case" <|> w "wrong") *> w "that"
_let = w "let"
_no = w "no"
_not = w "not"
_on = w "on"
_oneOf = w "one" *> w "of"
_or = w "or"
_relation = w "relation"
_satisfying = _suchThat <|> w "satisfying"
_setOf = w "set" *> w "of"
_show = optional (w "first" <|> w "finally" <|> w "next" <|> w "now") *> w "we" *> w "show" <* optional (w "that")
_since = w "since" <|> w "because"
_some = w "some"
_suchThat = w "such" *> w "that"
_suppose = w "suppose" <* optional (w "that")
_that = w "that"
_the = w "the"
_then = w "then"
_throughout = w "throughout" <* optional (w "this" *> w "section") <* optional _comma <|> (w "in" *> w "the" *> w "sequel")
_thus = w "thus"
_unique = w "unique"
_write = (optional (w "we") *> w "say" <* optional (w "that")) <|> (optional (w "we") *> w "write")


_colon = token (Symbol ":") <?> (":" :: String)
_comma = token (Symbol ",") <?> ("," :: String)
_commaAnd = token (Symbol ",") <* optional (w "and") <?> ("," :: String)
_commaOr = token (Symbol ",") <* optional (w "or") <?> ("," :: String)
_defeq = token (Symbol ":=") <?> (":=" :: String) -- Should use `\coloneq` from unicode-math as display.
_dot = token (Symbol ".") <?> ("." :: String)
_eq = token (Symbol "=") <?> ("=" :: String)
_in = token (Symbol "∈") <|> token (Command "in") <?> ("∈" :: String)