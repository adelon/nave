{-# LANGUAGE NoImplicitPrelude #-}

module Nave 
  ( tokenize, scan, parse, TokStream
  , simpleStream, exportLean, compileLean
  , builtins, extendLexicon
  , LException(..), ParseException(..)) where

import Base
import Grammar.Abstract (Para)
import Grammar.Concrete
import Grammar.Lexicon (builtins, Lexicon)
import Lex
import Scan
import Export.Lean (export, LException(..))

import Text.Earley (parser, fullParses, Report(..))
import Text.Megaparsec hiding (parse)

import qualified Data.Text as Text
import qualified Control.Exception as Exception

compileLean :: FilePath -> Text.Text -> IO (Either String Text.Text)
compileLean path raw = case tokenize path raw of
   Left e -> pure $ Left e
   Right stream -> case parse (extendLexicon (scan stream) builtins) stream of
      Left e -> pure $ Left $ displayException e
      Right ps -> do
         e <- exportLean ps
         case e of
            Left le -> pure $ Left $ displayException le
            Right t -> pure $ Right t

tokenize :: FilePath -> Text.Text -> Either String TokStream
tokenize path raw = case runParser toks path raw of
   Left err -> Left $ errorBundlePretty err
   Right stream -> Right (TokStream raw stream)

scan :: TokStream -> [ScanPattern]
scan stream = case fullParses (parser scanner) (simpleStream stream) of
   ([s], _) -> s
   _        -> impossible "scanner should have unambiguous grammar"

parse :: Lexicon -> TokStream -> Either ParseException [Para] 
parse lexicon stream = case fullParses (parser (grammar lexicon)) (simpleStream stream) of
   (_, r@(Report _ _ (_:_))) -> Left $ UnconsumedTokens r
   ([], _) -> Left $ EmptyParse
   (p@(_:_:_), _) -> Left $ AmbigousParse p
   ([p], _) -> Right p

simpleStream :: TokStream -> [Tok]
simpleStream stream = fmap unLocated (unTokStream stream)

exportLean :: [Para] -> IO (Either LException Text.Text)
exportLean ps = 
   Exception.try (Exception.evaluate $ export ps builtins)

data ParseException
   = UnconsumedTokens (Report String [Tok])
   | AmbigousParse [[Para]]
   | EmptyParse
   deriving (Eq, Show)

instance Exception ParseException