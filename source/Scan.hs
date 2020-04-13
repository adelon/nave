{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}

-- Pattern scanning for manual definitions from the preamble and for
-- automatic pattern recognition from definitions in the document.
--
module Scan where

import Base
import Grammar.Abstract
import Grammar.Concrete (env_)
import Grammar.Literals

import Text.Earley (Grammar, Prod, rule, satisfy)
import qualified Data.Set as Set




scanner :: Grammar r (Prod r String Tok [[Tok]])
scanner = do
   adjPat <- rule [p | _is, p <-  many1_ patToken, _iff]
   pat    <- rule (adjPat)
   defn   <- rule (env_ "definition" [p | many notDefnToken, p <- pat, many notDefnToken])
   doc    <- rule [ds | many notDefnToken, ds <- many defn, many notDefnToken]
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
      keywords = Set.fromList ["is", "are", "if", "iff"]
