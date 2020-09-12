{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

-- Pattern scanning for manual definitions from the preamble and for
-- automatic pattern recognition from definitions in the document.
--
module Scan where

import Base
import Grammar.Abstract
import Grammar.Concrete (env_, math, maybeVarTok)
import Grammar.Lexicon
import Grammar.Literals


import Text.Earley (Grammar, Prod, rule, satisfy, terminal)
import qualified Data.Set as Set
import qualified Data.Text as Text


data ScanPattern
   = ScanAttr Pattern
   | ScanFun Pattern
   | ScanNotion Pattern
   | ScanVerb Pattern
   deriving (Show, Eq)

-- The scanner is a grammar that picks out patterns from a document,
-- producing a list of all patterns. For use in the lexicon, singular
-- and plural forms still need to be derived for some patterns.
--
scanner :: Grammar r (Prod r String Tok [ScanPattern])
scanner = do
--
-- An expression consisting of a just a variable. They mark slots
-- of patterns which are represented by `Nothing`.
-- vvv
   var <- rule [Nothing | math (terminal maybeVarTok)]
--
-- A word, command, or grouping occuring within a pattern.
-- vvv
   word <- rule [Just w | w <- patToken ]
--
-- A complete `Pattern`, consisting of variable slots and pattern tokens.
-- vvv
   pat <- rule [p | p <- many1_ (var <|> word)]
--
-- Concrete patterns. The leading var and following keywords (`_is`/`_is, _an`)
-- serve to differentiate the different kinds of patterns.
-- vvvvvv
   attr   <- rule [ScanAttr p | var, _is, p <- pat, _iff]
   notion <- rule [ScanNotion p | var, _is, _an, p <- pat, _iff]
   verb   <- rule [ScanVerb p | var, p <- pat, _iff]
   scan   <- rule (attr <|> notion <|> verb)
--
-- We only care about the pattern content of definitions, and not the rest of the definition.
-- vvvv
   defn <- rule (env_ "definition" [p | many notDefnToken, p <- scan, many notDefnToken])
--
-- A version of `defn` that consumes all trailing tokens until the next `defn`.
-- vvvvv
   defn_ <- rule [p | p <- defn, many notDefnToken]
--
-- Strip all tokens before the first `defn_`, then collect all patterns from `defn_`s.
-- vvv
   doc <- rule [ps | many notDefnToken, ps <- many defn_]
   pure doc


notDefnToken :: Prod r e Tok Tok
notDefnToken = satisfy \case
   BeginEnv "definition" -> False
   EndEnv "definition"   -> False
   _token                -> True

notEnvToken :: Prod r e Tok Tok
notEnvToken = satisfy \case
   BeginEnv _ -> False
   EndEnv _   -> False
   _token     -> True

patToken :: Prod r e Tok Tok
patToken = satisfy \case
   Word w       -> w `Set.notMember` keywords
   --
   -- Simple commands (outside of math-mode) are allowed. This is useful
   -- for defining patterns containing symbolic expressions such as
   -- `X is \Ttwo{}`, where `\Ttwo` is a macro that expands to `T_2`.
   -- We also allow these macros to take arguments, hence the need to
   -- allow grouping delimiters. They can also be used to escape the end
   -- of the command for correct spacing, as in the above example.
   --
   Command _cmd -> True
   Open Invis   -> True
   Close Invis  -> True
   --
   -- No other tokens may occur in patterns. In particular, no `_dot`
   -- token may occur, limiting the pattern to a single sentence.
   -- Commas occurring in variable lists should be placed
   -- within the math environment. Thus `$a,b$ are coprime iff`,
   -- not `$a$,`$b$` are coprime iff`.
   --
   _token       -> False
   where
      keywords = Set.fromList ["a", "an", "is", "are", "if", "iff"]

-- Basic paradigms for pluralizations of nominals.
-- TODO Some kind of regex approach or using is less painful.
--
guessNominalPlural :: Pattern -> SgPl Pattern
guessNominalPlural pat = SgPl pat (pluralize pat)
   where
      pluralize :: Pattern -> Pattern
      pluralize = \case
         Just (Word w) : pat'@(Just w' : _) | isPreposition w' -> Just (Word (Text.snoc w 's')) : pat'
         [Just (Word w)] -> [Just (Word (Text.snoc w 's'))]
         [tok, Just (Word w)] -> [tok, Just (Word (Text.snoc w 's'))]
         [tok, tok', Just (Word w)] -> [tok, tok', Just (Word (Text.snoc w 's'))]
         pat' -> pat'

isAttrR :: Pattern -> Bool
isAttrR = containsPreposition
   where
      containsPreposition :: Pattern -> Bool
      containsPreposition pat = any isPreposition (catMaybes pat)

isPreposition :: Tok -> Bool
isPreposition w = Set.member w (Set.map Word prepositions)


-- Takes the scanned patterns and inserts them in the correct
-- places in a lexicon.
--
extendLexicon :: [ScanPattern] -> Lexicon -> Lexicon
extendLexicon [] lexicon = lexicon
--
--        Only add patterns that are fresh to avoid duplication.
--                                                 vvvvvvvvvvvvv
extendLexicon (scan : scans) lexicon@Lexicon{..} | notFresh scan = extendLexicon scans lexicon
   where
      notFresh :: ScanPattern -> Bool
      notFresh = \case
         ScanAttr pat -> Set.member pat lexiconAttrLs || Set.member pat lexiconAttrRs
         ScanNotion pat -> Set.member pat (Set.map sg lexiconNoms)
         ScanFun pat -> Set.member pat (Set.map sg lexiconFuns)
         ScanVerb pat -> Set.member pat (Set.map sg lexiconVerbs)
--
extendLexicon (scan : scans) lexicon@Lexicon{..} = case scan of
   ScanAttr pat | isAttrR pat -> extendLexicon scans lexicon{lexiconAttrRs = Set.insert pat lexiconAttrRs}
   ScanAttr pat -> extendLexicon scans lexicon{lexiconAttrLs = Set.insert pat lexiconAttrLs}
   --
   -- TODO adding patterns with grammatical number (SgPL).
   --
   _ -> extendLexicon scans lexicon
