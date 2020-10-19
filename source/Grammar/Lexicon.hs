{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
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
import Grammar.Pattern (unsafeReadPattern, unsafeReadPatternSgPl)

import Text.Earley.Mixfix (Holey, Associativity(..))

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text


data Lexicon = Lexicon
   { lexiconIdens       :: Set Tok
   , lexiconOperators   :: [[(Holey Tok, Associativity)]]
   , lexiconIsolOps     :: Set Tok
   , lexiconConnectives :: [[(Holey Tok, Associativity)]]
   , lexiconRelators    :: Set Relator
   , lexiconVerbs       :: Set (SgPl Pattern)
   , lexiconAdjLs      :: Set Pattern
   , lexiconAttrRs      :: Set Pattern
   , lexiconNoms        :: Set (SgPl Pattern)
   , lexiconFuns        :: Set (SgPl Pattern)
   } deriving (Show, Eq)

-- Projection returning the union of both left and right attributes.
--
lexiconAttr :: Lexicon -> Set Pattern
lexiconAttr lexicon = lexiconAdjLs lexicon <> lexiconAttrRs lexicon



builtins :: Lexicon
builtins = Lexicon
   { lexiconIdens       = builtinIdens
   , lexiconOperators   = builtinOperators
   , lexiconIsolOps     = builtinIsolOperators
   , lexiconConnectives = builtinConnectives
   , lexiconRelators    = builtinRelators
   , lexiconAdjLs      = builtinAdjLs
   , lexiconAttrRs      = builtinAttrRs
   , lexiconVerbs       = builtinVerbs
   , lexiconNoms        = builtinNominals
   , lexiconFuns        = builtinFuns
   }

-- Identifiers.
builtinIdens :: Set Tok
builtinIdens = Set.fromList
   [ Command "naturals"
   , Command "powerset"
   , Command "quotient"
   , Command "rationals"
   , Command "reals"
   , Command "reduc"
   , Command "unit"
   , Command "rankOne", Command "rankTwo", Command "rankThree", Command "rankFour", Command "rankFive", Command "rankSix", Command "rankSeven", Command "rankEight"
   , Command "fileA", Command "fileB", Command "fileC", Command "fileD", Command "fileE", Command "fileF", Command "fileG", Command "fileH"
   ]


builtinOperators :: [[(Holey Tok, Associativity)]]
builtinOperators =
   -- Using the names from Lean for now (lub, glb, ...).
   [ [(binOp (Command "glb"), LeftAssoc)]
   , [(binOp (Command "lub"), LeftAssoc)]
   , [(binOp (Command "directSum"), LeftAssoc)]
   , [(binOp (Command "tensor"), LeftAssoc)]
   , [(binOp (Symbol "+"), LeftAssoc)]
   , [(binOp (Command "circ"), LeftAssoc)]
   , [(binOp (Command "mul"), LeftAssoc)]
   ]


builtinIsolOperators :: Set Tok
builtinIsolOperators = Set.fromList [Command "mul", Command "unit", Symbol "+"]


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


builtinAdjLs :: Set Pattern
builtinAdjLs = Set.map unsafeReadPattern (Set.fromList
   [ "associative"
   , "confluent"
   , "even"
   , "injective"
   , "locally confluent"
   , "monotone"
   , "nonzero"
   , "odd"
   , "reflexive"
   , "surjective"
   , "terminating"
   , "transitive"
   , "well founded"
   ])

builtinAttrRs :: Set Pattern
builtinAttrRs = Set.map unsafeReadPattern (Set.fromList
   [ "of finite order"
   , "of finite type"
   , "pointwise bounded by ? on ?"
   , "vertically adjacent to ?"
   , "horizontally adjacent to ?"
   , "adjacent to ?"
   ])


builtinVerbs :: Set (SgPl Pattern)
builtinVerbs = Set.map unsafeReadPatternSgPl (Set.fromList
   [ "converge[s/]"
   , "divide[s/] ?"
   , "equal[s/] ?"
   , "reduce[s/] to ?"
   , "agree[s/] with ? on ?"
   ])


-- Some of these do/should correspond to mathlib structures,
-- e.g.: lattice, complete lattice, ring, etc.
--
builtinNominals :: Set (SgPl Pattern)
builtinNominals = Set.map unsafeReadPatternSgPl (Set.fromList
   -- Nullary
   [ "arithmetic[/s]"
   , "complete lattice[/s]"
   , "domino[/s]"
   , "field[/s]"
   , "file[/s]"
   , "geometr[y/ies]"
   , "group[/s]"
   , "lattice[/s]"
   , "magma[/s]"
   , "monoid[/s]"
   , "natural number[/s]"
   , "rank[/s]"
   , "rational number[/s]"
   , "rewriting system[/s]"
   , "ring[/s]"
   , "semigroup[/s]"
   , "set[/s]"
   , "square[/s]"
   , "tiling[/s]"
   , "unital magma[/s]"
   -- Unary
   , "divisor[/s] of ?"
   , "endomorphism[/s] of ?"
   , "fixed point[/s] of ?"
   , "ideal[/s] of ?"
   , "linear form[/s] on ?"
   , "relation[/s] on ?"
   , "seminorm[/s] on ?"
   , "subspace[/s] of ?"
   , "vector space[/s] over ?"
   -- Binary
   , "pair of ? and ?"
   , "magma homomorphism[/s] from ? to ?"
   ])


builtinFuns :: Set (SgPl Pattern)
builtinFuns = Set.map unsafeReadPatternSgPl (Set.fromList
   -- Nullary
   [ "checkerboard[/s]"
   , "mutilated checkerboard[/s]"
   -- Unary
   , "derivative[/s] of ?"
   , "successor[/s] of ?"
   , "tensor product[/s] of ? and ?"
   , "transitive closure[/s] of ?"
   , "reflexive transitive closure[/s] of ?"
   , "infim[um/a] of ?"
   , "dual of ?"
   , "space of bilinear forms on ?"
   , "set[/s] of fixed points of ?"
   ])


-- Naïve splitting of patterns to insert a variable slot for notions, as in
-- 'there exists a linear form $h$ on $E$', where the underlying pattern is
-- 'linear form on ?'. In this case we would get:
--
-- splitOnVariableSlot (sg (unsafeReadPatternSgPl "linear form[/s] on ?"))
-- ==
-- (unsafeReadPattern "linear form", unsafeReadPattern "on ?")
--
splitOnVariableSlot :: Pattern -> (Pattern, Pattern)
splitOnVariableSlot pat = case prepositionIndices <> nonhyphenatedSlotIndices of
   [] -> (pat, []) -- Place variable slot at the end.
   is -> List.splitAt (minimum is) pat
   where
      prepositionIndices, slotIndices, nonhyphenatedSlotIndices :: [Int] -- Ascending.
      prepositionIndices = List.findIndices isPreposition pat
      slotIndices = List.findIndices isNothing pat
      nonhyphenatedSlotIndices = [i | i <- slotIndices, noHyphen (nth (i + 1) pat)]

      isPreposition :: Maybe Tok -> Bool
      isPreposition = \case
         Just (Word w) -> w `Set.member` prepositions
         _ -> False

      noHyphen :: Maybe (Maybe Tok) -> Bool
      noHyphen = \case
         Just (Just (Word w)) -> Text.head w /= '-'
         -- If we arrive here, either the pattern is over (`Nothing`) or the next
         -- part of the pattern is not a word that starts with a hyphen.
         _ -> True


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
   , "along", "alongside"
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
   , "except"
   , "for"
   , "from"
   , "in", "inside", "into"
   , "like"
   , "modulo", "mod"
   , "near"
   , "next"
   , "of"
   , "off"
   , "on"
   , "onto"
   , "opposite"
   , "out"
   , "over"
   , "past"
   , "per"
   , "sans"
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
