{-# LANGUAGE NoImplicitPrelude #-}
-- This module defines the lexer and its associated data types.
-- The lexer takes `Text` as input and produces a stream of tokens
-- annotated with positional information. This information is bundled
-- together with the original input raw input for producing error messages.
--
-- The lexer perfoms some normalizations to make describing the grammar easier.
-- Words outside of math environments are case-folded. Some commands are analyzed
-- as variable tokens and are equivalent to their respective unicode variants
-- (α, β, γ, ..., 𝔸, 𝔹, ℂ, ...). Similarly, `\begin{...}` and `\end{...}` commands
-- are parsed as a single token.
--

module Lex
   ( Tok(..)
   , Delim(..)
   , TokStream(..)
   , Located(..)
   , toks
   , printTok
   ) where


import Base hiding (try, many)

import Text.Megaparsec

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer


type Lexer = Parsec Void Text

-- |
-- A token stream for as input stream for a parser. Contains the raw input
-- before tokenization as @Text@ for showing error messages.
--
data TokStream = TokStream
   { rawInput :: Text
   , unTokStream :: [Located Tok]
   } deriving (Show, Eq)

data Tok
   = Word Text
   | Variable Text
   | Symbol Text
   | Number Text
   | Command Text
   | BeginEnv Text
   | EndEnv Text
   | Open Delim
   | Close Delim
   deriving (Show, Eq, Ord)

instance IsString Tok where
   fromString w = Word (Text.pack w)

-- | Invisible delimiters are plain braces used for grouping in TEX,
-- braces are escaped braces.
data Delim = Invis | Paren | Brace | Bracket deriving (Show, Eq, Ord)

printTok :: Tok -> Text
printTok = \case
   Word w -> w
   Variable v -> v
   Symbol s -> s
   Number n -> n
   Command cmd -> Text.cons '\\' cmd
   BeginEnv env -> "\\begin{" <> env <> "}"
   EndEnv env -> "\\end{" <> env <> "}"
   Open delim -> case delim of
      Invis -> "{"
      Paren -> "("
      Brace -> "\\{"
      Bracket -> "["
   Close delim -> case delim of
      Invis -> "}"
      Paren -> ")"
      Brace -> "\\}"
      Bracket -> "]"


data Located a = Located
   { startPos :: SourcePos
   , endPos :: SourcePos
   , tokenLength :: Int
   , unLocated :: a
   } deriving (Show)

instance Eq a  => Eq  (Located a) where (==) = (==) `on` unLocated
instance Ord a => Ord (Located a) where compare = compare `on` unLocated

-- | Parses tokens, switching tokenizing mode when encountering math environments.
toks :: Lexer [Located Tok]
toks = space *> go id
   where
      -- Instead of adding explicit state to our Lexer we implement a token parser
      -- using two mutually recursive helper functions.
      go f = do
         r <- optional tok
         case r of
            Nothing -> pure (f [])
            Just t@(Located _ _ _ (BeginEnv "math")) -> go' (f . (t:))
            Just t -> go (f . (t:))
      go' f = do
         r <- optional mathTok
         case r of
            Nothing -> pure (f [])
            Just t@(Located _ _ _ (EndEnv "math")) -> go (f . (t:))
            Just t -> go' (f . (t:))
{-# INLINE toks #-}

-- | Parses a single normal mode token.
tok :: Lexer (Located Tok)
tok = word <|> symbol <|> begin <|> end <|> open <|> close <|> command <|> mathBegin

-- | Parses a single math mode token.
mathTok :: Lexer (Located Tok)
mathTok = var <|> symbol <|> number <|> begin <|> end <|> open <|> close <|> command <|> mathEnd

-- | Parses a single begin math token.
mathBegin :: Lexer (Located Tok)
mathBegin = lexeme do
   try (Char.string "\\(" <|> Char.string "\\[" <|> Char.string "$")
   pure (BeginEnv "math")

-- | Parses a single end math token.
mathEnd :: Lexer (Located Tok)
mathEnd = lexeme do
   try (Char.string "\\)" <|> Char.string "\\]" <|> Char.string "$")
   pure (EndEnv "math")

-- | Parses a word. Words are returned casefolded, since we want to ignore their case later on.
word :: Lexer (Located Tok)
word = lexeme do
   w <- some (Char.letterChar <|> Char.char '\'')
   let t = Word (Text.toCaseFold (Text.pack w))
   pure t

number :: Lexer (Located Tok)
number = lexeme do
   n <- some Char.digitChar
   let t = Number (Text.pack n)
   pure t

var :: Lexer (Located Tok)
var = lexeme (fmap Variable (letter <|> bb <|> greek))
   where
   letter :: Lexer Text
   letter = fmap Text.singleton Char.letterChar

   greek :: Lexer Text
   greek = try do
      Char.char '\\'
      l <- asum (fmap makeSymbolParser greeks)
      notFollowedBy Char.letterChar
      pure l

   greeks :: [(Text,Text)]
   greeks =
      [ ("alpha", "α"), ("beta", "β"), ("gamma", "γ")
      , ("delta", "δ"), ("epsilon", "ε"), ("zeta", "ζ")
      , ("eta", "η"), ("theta", "θ"), ("iota", "ι")
      , ("kappa", "κ"), ("lambda", "λ"), ("mu", "μ")
      , ("nu", "ν"), ("xi", "ξ"), ("pi", "π")
      , ("rho", "ρ"), ("sigma", "σ"), ("tau", "τ")
      , ("upsilon", "υ"), ("phi", "φ"), ("chi", "χ")
      , ("psi", "ψ"), ("omega", "ω")
      , ("Gamma", "Γ"), ("Delta", "Δ"), ("Theta", "Θ")
      , ("Lambda", "Λ"), ("Xi", "Ξ"), ("Pi", "Π")
      , ("Sigma", "Σ"), ("Upsilon", "Υ"), ("Phi", "Φ")
      , ("Psi", "Ψ"), ("Omega", "Ω")
      ]

   bb :: Lexer Text
   bb = do
      try (Char.string "\\mathbb{")
      l <- asum (fmap makeSymbolParser bbs)
      Char.char '}'
      pure l

   bbs :: [(Text,Text)]
   bbs =
      [ ("A", "𝔸")
      , ("B", "𝔹")
      , ("C", "ℂ")
      , ("N", "ℕ")
      , ("P", "ℙ")
      , ("Q", "ℚ")
      , ("R", "ℝ")
      , ("Z", "ℤ")
      ]

   makeSymbolParser :: (Text, b) -> Lexer b
   makeSymbolParser (cmd, symb) = do
      Char.string cmd
      pure symb

symbol :: Lexer (Located Tok)
symbol = lexeme do
   symb <- some (satisfy (`elem` symbols))
   pure (Symbol (Text.pack symb))
      where
      symbols :: [Char]
      symbols = ".,:;!?@=≠+-/^><≤≥*&≈⊂⊃⊆⊇∈"

-- | Parses a TEX-style command.
command :: Lexer (Located Tok)
command = (lexeme . try) do
  Char.char '\\'
  cmd <- some Char.letterChar
  pure (Command (Text.pack cmd))

-- | Parses the beginning of an environment. Commits only after having seen "\begin{".
begin :: Lexer (Located Tok)
begin = lexeme do
  try (Char.string "\\begin{")
  env <- some Char.letterChar
  Char.char '}'
  pure (BeginEnv (Text.pack env))

-- | Parses the end of an environment. Commits only after having seen "\end{".
end :: Lexer (Located Tok)
end = lexeme do
  try (Char.string "\\end{")
  env <- some Char.letterChar
  Char.char '}'
  pure (EndEnv (Text.pack env))

-- | Parses an opening delimiter.
open :: Lexer (Located Tok)
open = lexeme (paren <|> brace <|> group <|> bracket)
   where
      brace = Open Brace <$ lexeme (try (Char.string "\\{"))
      group = Open Invis <$ lexeme (Char.char '{')
      paren = Open Paren <$ lexeme (Char.char '(')
      bracket = Open Bracket <$ lexeme (Char.char '[')

-- | Parses a closing delimiter.
close :: Lexer (Located Tok)
close = lexeme (paren <|> brace <|> group <|> bracket)
   where
      brace = Close Brace <$ lexeme (try (Char.string "\\}"))
      group = Close Invis <$ lexeme (Char.char '}')
      paren = Close Paren <$ lexeme (Char.char ')')
      bracket = Close Bracket <$ lexeme (Char.char ']')

-- | Turns a Lexer into one that tracks the source position of the token
-- and consumes trailing whitespace.
lexeme :: Lexer a -> Lexer (Located a)
lexeme p = do
   start <- getSourcePos
   startOffset <- getOffset
   t <- p
   space
   stop <- getSourcePos
   stopOffset <- getOffset
   let l = stopOffset - startOffset
   pure (Located start stop l t)

space :: Lexer ()
space = Lexer.space Char.space1 (Lexer.skipLineComment "%") (empty)
