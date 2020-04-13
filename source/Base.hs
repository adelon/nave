-- A basic prelude intended to reduce the amount of repetitive imports.
-- Mainly consists of re-exports from `base` modules, via `BasePrelude`.
-- Intended to be imported explicitly (but with `NoImplicitPrelude` enabled)
-- so that it is obvious that this module is used. Commonly used data types
-- or helper functions from outside of `base` are also included.
--
module Base (module Base, module Export) where

-- Some definitions from `base` need to be hidden to avoid clashes.
--
import BasePrelude as Export hiding
   ( Word, TypeError, Const
   , (<+>)
   , head, last, init, tail, lines
   , words, pi, app, some, All, all
   , group, bracket
   )

import Data.Set as Export (Set)
import Data.Text as Export (Text)
import Data.Containers.ListUtils as Export (nubOrd) -- Much faster than `nub`.

import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Applicative as Applicative

-- Eliminate a `Maybe a` value with default value as fallback.
--
(??) :: Maybe a -> a -> a
ma ?? a = fromMaybe a ma

-- Do nothing and return `()`.
--
pass :: Applicative f => f ()
pass = pure ()

-- One or more. Equivalent to `some` from `Control.Applicative`, but
-- keeps the information that the result is `NonEmpty`.
--
many1 :: Alternative f => f a -> f (NonEmpty a)
many1 a = NonEmpty.fromList <$> Applicative.some a

-- Discards the type information that the result is `NonEmpty`.
many1_ :: Alternative f => f a -> f [a]
many1_ = Applicative.some
