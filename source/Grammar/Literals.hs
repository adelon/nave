{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- This module defines lots of keywords, introduction sequences
-- and various filler phrases. Collectively we call them 'literals'.
-- The prefix underscore `_` should indicate that we typically do
-- not care about the parse result of literals by being reminiscent
-- of the explicit discard in do-notation `...; _ <- action; ...`.
-- Moreover, this convention allows us to use short names that
-- would otherwise be Haskell keywords (e.g.: `_let`, `_of`)
-- or clash with other definitions (e.g.: `_and, _or, _show`).
-- Names of literals are chosen by the the following criteria:
--
-- * Be as short as possible (e.g.: `_since` over `_because`).
-- * Sound like a keyword (e.g.: `_show`).
-- * Avoid confusion with other literals.
--
-- This module also defines symbols for parsing formulae
-- with special meaning (typings, variable assignments, etc.).
--
-- Since we do not care about the result type, we omit type
-- signatures in this module. Definitions are suppose to be
-- in alphabetical order. The explicit export list serves to
-- hide the ad-hoc shortening of `word` to `w`.
--
-- Care should be taken with introducing too many variants of
-- a literal, lest the grammar becomes needlessly ambiguous!
--
module Grammar.Literals
   ( _all
   , _an
   , _and
   , _are
   , _be
   , _equipped
   , _every
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
   , _or
   , _satisfying
   , _show
   , _since
   , _suchThat
   , _suppose
   , _the
   , _then
   , _throughout
   , _thus
   , _write
   -- Symbols
   , _colon
   , _comma
   , _defeq
   , _dot
   , _eq
   , _in
   ) where


import Base
import Grammar.Abstract (Tok(..))

import Text.Earley ((<?>), token)

import qualified Data.Text as Text


w lit = token (Word lit) <?> Text.unpack lit


_all = optional (w "for") *> w "all"
_an = w "a" <|> w "an"
_and = w "and"
_are = w "are"
_be = w "be"
_equipped = optional (w "equipped") *>  "with"
_every = optional (w "for") *> w "every"
_exists = w "there" *> w "exists"
_extends = _is *> _an
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
_or = w "or"
_satisfying = _suchThat <|> w "satisfying"
_show = optional (w "first" <|> w "finally" <|> w "next" <|> w "now") *> w "we" *> w "show" <* optional (w "that")
_since = w "since" <|> w "because"
_suchThat = w "such" *> w "that"
_suppose = w "suppose" <* optional (w "that")
_the = w "the"
_then = w "then"
_throughout = (w "throughout" <* optional (w "this" *> w "section") <|> (w "in" *> w "the" *> w "sequel"))
_thus = w "thus"
_write = (optional (w "we") *> w "say" <* optional (w "that")) <|> (optional (w "we") *> w "write")


_colon = token (Symbol ":") <?> ":"
_comma = token (Symbol ",") <?> ","
_defeq = token (Symbol ":=") <?> ":=" -- Should use `\coloneq` from unicode-math as display.
_dot = token (Symbol ".") <?> "."
_eq = token (Symbol "=") <?> "="
_in = token (Symbol "∈") <|> token (Command "in") <?> "∈"
