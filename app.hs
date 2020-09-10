module Main where

import Base
import Grammar.Concrete
import Grammar.Lexicon (builtins)
import Lex
import Scan

import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import Text.Earley (parser, fullParses)
import Text.Megaparsec
import Text.Pretty.Simple (pShowNoColor)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LazyText

main :: IO ()
main = do
   let createParents = True
   createDirectoryIfMissing createParents "debug/"
   files <- getFiles "examples/"
   mapM_ work files


getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
   contents <- getDirectoryContents dir
   pure (filter (".tex" `isSuffixOf`) contents)

work :: FilePath -> IO ()
work file = do
   let inPath = "examples/" <> file
   let outPath = "debug/" <> file <> ".out"
   let tokPath = "debug/" <> file <> ".tokens"
   let lexiconPath = "debug/" <> file <> ".lexicon"
   let scanPath = "debug/" <> file <> ".scans"
   putStrLn ("Parsing '" <> inPath <> "'.")
   tokenResult <- tokenize inPath
   case tokenResult of
      Left err -> Text.writeFile tokPath (Text.pack (errorBundlePretty err))
      Right stream -> do
         --
         -- Dump token stream into a file for debugging.
         Text.writeFile tokPath (dumpTokens stream)
         --
         -- Remove raw source and location information for now.
         let simpleStream = fmap unLocated (unTokStream stream)
         let scanResult = fullParses (parser scanner) simpleStream
         let scans = case scanResult of
               ([s], _) -> s
               _        -> impossible "scanner should have unambiguous grammar"
         --
         -- Write scanned patterns to a file.
         Text.writeFile scanPath (Text.pack (show scanResult))
         --
         -- Update the lexicon and dump its contents for debugging.
         let lexicon = extendLexicon scans builtins
         LazyText.writeFile lexiconPath (pShowNoColor lexicon)
         --
         -- Write the parse tree to a file.
         let parseResult = fullParses (parser (grammar lexicon)) simpleStream
         LazyText.writeFile outPath (pShowNoColor parseResult)


dumpTokens :: TokStream -> Text
dumpTokens = Text.pack . show . fmap unLocated . unTokStream

tokenize :: FilePath -> IO (Either (ParseErrorBundle Text Void) TokStream)
tokenize path = do
   raw <- Text.readFile path
   let result = runParser toks path raw
   case result of
      Left err -> pure (Left err)
      Right stream -> pure (Right (TokStream raw stream))
