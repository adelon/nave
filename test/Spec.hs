-- Start the test procedure by running `stack test` in
-- the project directory. To load the test modules in
-- the REPL run `stack repl nave:nave-test`.

import Test.Hspec

import Base

import qualified LexerSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "Lexer" LexerSpec.spec
