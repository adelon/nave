{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Grammar.Pattern where


import Base
import Lex (Tok(..), Delim(..))


import Text.Earley.Mixfix (Holey)
import Text.Earley (Grammar, Prod, (<?>), fullParses, parser, rule, token, satisfy)
import qualified Data.Text as Text




-- Patterns should be nonempty lists with at least one proper word token.
-- Hyphens and quotes in words are treated as letters.
-- Thus 'manifold-with-boundary' is a singleton pattern.
--
type Pattern = Holey Tok

-- Split data by grammatical number.
data SgPl a = SgPl {sg :: a, pl :: a} deriving (Show, Eq, Ord, Functor)


unsafeReadPattern :: String -> Pattern
unsafeReadPattern spec = case (fst (fullParses (parser patternSpec) spec)) of
   pat : _ -> pat
   _ -> error "unsafeReadPattern failed"

unsafeReadPatternSgPl :: String -> SgPl Pattern
unsafeReadPatternSgPl spec = case (fst (fullParses (parser patternSpecSgPl) spec)) of
   pat : _ -> pat
   _ -> error "unsafeReadPatternSgPl failed"


-- TODO: The pattern spec grammars are the result of sloppy Type Tetris (TM).
-- They work for specifying builtin patterns, but a user-facing
-- pattern scanner should be a bit more robust.

patternSpec :: Grammar r (Prod r String Char Pattern)
patternSpec = do
   hole     <- rule ([Nothing | token '?'] <?> "hole")
   word     <- rule [Just cs | cs <- many (satisfy isAlpha)]
   space    <- rule [Just [c] | c <- token ' ']
   segment  <- rule (hole <|> word)
   segments <- rule [makePattern (s:ss) | s <- segment, ss <- many (space *> segment)]
   pure segments
   where
      makePattern :: [Maybe String] -> Pattern
      makePattern pat = fmap makeWord pat


patternSpecSgPl :: Grammar r (Prod r String Char (SgPl Pattern))
patternSpecSgPl = do
   space <- rule [Just [c] | c <- token ' ']
   hole  <- rule ([(Nothing, Nothing) | token '?'] <?> "hole")

   word <- rule (many (satisfy isAlpha) <?> "word")
   wordSgPl <- rule [(wSg, wPl) | token '[', wSg <- word, token '/', wPl <- word, token ']']
   complexWord <- rule $ (\(a,b) -> (Just a, Just b)) . fuse <$>
      many ((<>) <$> (dup <$> word) <*> wordSgPl) <?> "word"
   segment  <- rule (hole <|> [dup (Just w) | w <- word] <|> complexWord )
   segments <- rule [makePattern (s:ss) | s <- segment, ss <- many (space *> segment)]
   pure segments
   where
      dup x = (x,x)
      fuse = \case
         (a, b) : (c, d) : rest -> fuse ((a <> c, b <> d) : rest)
         (a, b) : [] -> (a, b)
         _ -> error "Grammar.Abstract.fuse"

      makePattern :: [(Maybe String, Maybe String)] -> SgPl Pattern
      makePattern = (\(patSg, patPl) -> SgPl (fmap makeWord patSg) (fmap makeWord patPl)) . unzip

makeWord :: Maybe String -> Maybe Tok
makeWord = fmap (Word . Text.pack)
