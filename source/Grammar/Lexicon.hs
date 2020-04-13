-- The `Lexicon` describes the part of the grammar that extensible/dynamic.
-- It starts with built-ins based on Lean's `stdlib` and `mathlib`.
-- It be extended by the user.
--
-- The items of the `Lexicon` are organized by their meaning and their
-- syntactic behaviour. They are typically represented as some kind of
-- pattern data which is then used to generate various production rules
-- for the concrete grammar. This representation makes inspection and
-- extension easier.
--
module Grammar.Lexicon where


import Base
import Grammar.Abstract

import Text.Earley.Mixfix (Holey, Associativity(..))

import qualified Data.Set as Set
import qualified Data.List as List


data Lexicon = Lexicon
   { lexiconIdens :: Set Tok
   , lexiconOperators :: [[(Holey Tok, Associativity)]]
   , lexiconConnectives :: [[(Holey Tok, Associativity)]]
   , lexiconRelators :: Set Relator
   , lexiconVerbs :: [SgPl Pattern]
   , lexiconAttrs :: [Pattern]
   , lexiconNoms :: [SgPl Pattern]
   , lexiconFuns :: [SgPl Pattern]
   } deriving (Show, Eq)


builtins :: Lexicon
builtins = Lexicon
   { lexiconIdens = builtinIdens
   , lexiconOperators = builtinOperators
   , lexiconConnectives = builtinConnectives
   , lexiconRelators = builtinRelators
   , lexiconAttrs = builtinAttrs
   , lexiconVerbs = builtinVerbs
   , lexiconNoms = builtinNominals
   , lexiconFuns = builtinFuns
   }

-- Identifiers.
builtinIdens :: Set Tok
builtinIdens = Set.fromList
   [ Command "naturals"
   , Command "rationals"
   , Command "reals"
   , Command "quotient"
   , Command "powerset"
   ]


builtinOperators :: [[(Holey Tok, Associativity)]]
builtinOperators =
   -- Using the names from Lean for now (lub, glb, ...).
   [ [(binOp (Command "glb"), LeftAssoc)]
   , [(binOp (Command "lub"), LeftAssoc)]
   , [(binOp (Symbol "+"), LeftAssoc)]
   , [(binOp (Command "mul"), LeftAssoc)]
   ]


builtinRelators :: Set Relator
builtinRelators = Set.fromList
   [ Symbol "="
   , Symbol "≠"
   , Command "neq"
   , Symbol "<"
   , Symbol ">"
   , Symbol "≤"
   , Symbol "≥"
   , Symbol "⊂"
   , Symbol "⊃"
   , Symbol "⊆"
   , Symbol "⊇"
   , Symbol "≈"
   , Symbol "~"
   , Symbol "∈"
   , Command "divides"
   , Command "ringEquiv"
   ]


builtinConnectives :: [[(Holey Tok, Associativity)]]
builtinConnectives =
   [ [(binOp (Command "to"), RightAssoc)]
   , [(binOp (Command "lor"), LeftAssoc)]
   , [(binOp (Command "land"), LeftAssoc),(binOp (Command "times"), LeftAssoc)]
   , [([Nothing, Just(Command "lnot")], NonAssoc)]
   ]


binOp :: Tok -> Holey Tok
binOp tok = [Nothing, Just tok, Nothing]


builtinAttrs :: [Pattern]
builtinAttrs = fmap unsafeReadPattern
   [ "associative"
   , "even"
   , "injective"
   , "monotone"
   , "nonzero"
   , "odd"
   , "prime"
   , "reflexive"
   , "surjective"
   , "transitive"
   , "coprime to ?"
   , "pointwise bounded by ? on ?"
   ]


builtinVerbs :: [SgPl Pattern]
builtinVerbs = fmap unsafeReadPatternSgPl
   [ "converge[s/]"
   , "divide[s/] ?"
   , "equal[s/] ?"
   , "agree[s/] with ? on ?"
   ]


-- Some of these do/should correspond to mathlib structures,
-- e.g.: lattice, complete lattice, ring, etc.
--
builtinNominals :: [SgPl Pattern]
builtinNominals = fmap unsafeReadPatternSgPl
   [ "lattice[/s]"
   , "complete lattice[/s]"
   , "natural number[/s]"
   , "rational number[/s]"
   , "ring[/s]"
   , "set[/s]"
   , "divisor[/s] of ?"
   , "endomorphism[/s] of ?"
   , "fixed point[/s] of ?"
   , "ideal[/s] of ?"
   , "linear form[/s] on ?"
   , "seminorm[/s] on ?"
   , "subspace[/s] of ?"
   , "vector space[/s] over ?"
   ]


builtinFuns :: [SgPl Pattern]
builtinFuns = fmap unsafeReadPatternSgPl
   [ "derivative[/s] of ?"
   , "successor[/s] of ?"
   , "infim[um/a] of ?"
   , "set[/s] of fixed points of ?"
   ]


-- Naïve splitting of patterns at the first preposition.
-- This is intended for naming notions with variables, as in
-- 'there exists a linear form $h$ on $E$', where the underlying
-- pattern is 'linear form on ?'. In this case we would get:
--
-- splitOnPreposition (sg (unsafeReadPatternSgPl "linear form[/s] on ?"))
-- ==
-- (unsafeReadPattern "linear form", unsafeReadPattern "on ?")
--
splitOnPreposition :: Pattern -> (Pattern, Pattern)
splitOnPreposition = List.break isPreposition
   where
      isPreposition :: Maybe Tok -> Bool
      isPreposition = \case
         Just (Word w) -> w `Set.member` prepositions
         _ -> False


-- Preposition are a closed class, but this list is not yet exhaustive.
-- It can and should be extended when needed. The following list is a
-- selection of the prepositions found at
-- https://en.wikipedia.org/wiki/List_of_English_prepositions.
--
prepositions :: Set Text
prepositions = Set.fromList
   [ "about"
   , "above"
   , "across"
   , "after"
   , "against"
   , "along"
   , "alongside"
   , "amid", "amidst"
   , "among"
   , "around"
   , "as"
   , "at"
   , "atop"
   , "before"
   , "behind"
   , "below"
   , "beneath"
   , "beside", "besides"
   , "between"
   , "beyond"
   , "but"
   , "by"
   , "for"
   , "from"
   , "in", "inside", "into"
   , "near"
   , "of"
   , "off"
   , "on"
   , "onto"
   , "opposite"
   , "out"
   , "over"
   , "past"
   , "per"
   , "till"
   , "to"
   , "under"
   , "underneath"
   , "unlike"
   , "unto"
   , "up", "upon"
   , "versus"
   , "via"
   , "with"
   , "within"
   , "without"
   ]
