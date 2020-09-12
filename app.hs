module Main where

import Base
import qualified Nave
import Text.Pretty.Simple (pShowNoColor)

import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LazyText
import qualified Control.Exception as Exception

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
   let leanPath = "examples/" <> file <> ".lean"
   let tokPath = "debug/" <> file <> ".tokens"
   let lexiconPath = "debug/" <> file <> ".lexicon"
   let scanPath = "debug/" <> file <> ".scans"
   putStrLn ("Parsing '" <> inPath <> "'.")
   tokenResult <- Nave.tokenize inPath <$> Text.readFile inPath
   case tokenResult of
      Left err -> Text.writeFile tokPath (Text.pack err)
      Right stream -> do
         --
         -- Dump token stream into a file for debugging.
         Text.writeFile tokPath (dumpTokens stream)
         --
         -- Remove raw source and location information for now.
         let scans = Nave.scan stream
         --
         -- Write scanned patterns to a file.
         Text.writeFile scanPath (Text.pack (show scans))
         --
         -- Update the lexicon and dump its contents for debugging.
         let lexicon = Nave.extendLexicon scans Nave.builtins
         LazyText.writeFile lexiconPath (pShowNoColor lexicon)
         --
         -- Write the parse tree to a file.
         let parseResult = Nave.parse lexicon stream
         LazyText.writeFile outPath (pShowNoColor parseResult)
         --
         -- Translate to lean.
         case parseResult of
            Left _ -> pure ()
            Right ps -> do
               l <- Nave.exportLean ps
               case l of
                  Right lean -> Text.writeFile leanPath lean
                  Left e -> do
                     writeFile leanPath (Exception.displayException e)
                     putStrLn (Exception.displayException e)

dumpTokens :: Nave.TokStream -> Text
dumpTokens = Text.pack . show . Nave.simpleStream