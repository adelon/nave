{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}

-- Concrete grammar of the surface language.
--
-- This grammar specification makes use of applicative-comprehension syntax for
-- its production rules. This is particularly useful when there is a lot of shuffling
-- or unwrapping of production results, since these get cumbersome with the standard
-- applicative notation. Here is one gotcha: since pattern matches in do-notation and
-- comprehensions are strict by default, we need to make those pattern matches lazy
-- (by prepending a tilde), so that the applicative translation becomes possible.
--
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ApplicativeDo
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-comprehensions
-- https://mail.haskell.org/pipermail/ghc-devs/2015-October/010065.html
--
module Grammar.Concrete where

import Base
import Grammar.Abstract
import Grammar.Literals
import Grammar.Lexicon (Lexicon(..), lexiconAttr, splitOnPreposition)

import Text.Earley (Grammar, Prod, (<?>), rule, satisfy, terminal, token)
import Text.Earley.Mixfix (mixfixExpression)

import qualified Data.Set as Set
import qualified Data.Text as Text





grammar :: Lexicon -> Grammar r (Prod r String Tok [Para])
grammar lexicon@Lexicon{..} = mdo
   let ops   = map ((map . first . map . fmap) token) lexiconOperators
   let conns = map ((map . first . map . fmap) token) lexiconConnectives

   iden     <- rule (satisfy (`Set.member` lexiconIdens)    <?> "identifier")
   isolOp   <- rule (satisfy (`Set.member` lexiconIsolOps)  <?> "isolated operator")
   number   <- rule (terminal maybeNumberTok                <?> "number")
   relator  <- rule (satisfy (`Set.member` lexiconRelators) <?> "relator")
   var      <- rule (terminal maybeVarTok                   <?> "variable")
   vars     <- rule (commaSep var)
   cmd      <- rule (terminal maybeCmdTok                   <?> "TEX command")


-- Formulae have three levels:
--
-- * Expressions: atoms or operators applied to atoms.
-- * Chains:      comma-lists of expressions, separated by relators.
-- * Formulae:    chains or connectives applied to chains.
--
-- For example, the formula `x, y < z -> x, y < z + 1` consist of the
-- connective `->` applied to two chains `x, y < z` and `x,y < z + 1`.
-- In turn, the chain `x, y < z + 1` consist of three expressions,
-- `x`, `y`, and `z + 1`. Finally, `z + 1` consist the operator `+`
-- applied to two atoms, the variable `z` and the number literal `1`.
--
-- This split is due to the different behaviour of relators compared to
-- operators and connectives. Relators can chain (`x < y < z`) and allow
-- lists, as in the above example. Operators and connectives instead have
-- precedence and fixity. The only syntactic difference between an operator
-- and a connective is the relative precedence compared to relators. Thus
-- connectives need not only be 'logical operations'.
--
   exprParen   <- rule [ExprParen e | e <- paren formula <|> group formula]
   exprConst   <- rule [ExprConst t | t <- iden]
   exprNumber  <- rule [ExprNumber n | n <- number]
   exprVar     <- rule [ExprVar x | x <- var]
   exprBase    <- rule (exprVar <|> exprConst <|> exprNumber <|> exprParen)
   exprApp     <- rule [ExprApp e1 e2 | e1 <- exprBase, e2 <- paren formula <|> group formula]
   expr        <- mixfixExpression ops (exprBase <|> exprApp) ExprOp
   exprs       <- rule (commaSep expr)

   chainBase   <- rule [ChainBase es | es <- exprs]
   chainCons   <- rule [ChainCons es r c | es <- exprs, r <- relator, c <- chain]
   chain       <- rule (chainCons <|> chainBase)

   formulaBase <- rule [ExprChain c | c <- chain]
   formula     <- mixfixExpression conns formulaBase ExprOp

-- These are asymmetric formulae, in the sense that we only allow
-- variables on one side. They express judgements.
--
   assignment  <- rule [(x, e) | x <- var, _eq <|> _defeq, e <- formula]
   typing      <- rule [(xs, e) | xs <- vars, _in, e <- formula]


-- Patterns always allow full terms as arguments. If stricter conditions are
-- to be placed on the arguments, then they have to verified with a
-- well-formedness check, not grammatically. This simplifies the grammar and
-- lets us give more specific error messages, since it is no longer a generic
-- 'unexpected <token>' error.
--
   attrL       <- rule (attrLOf lexicon term)
   attrSuch    <- rule [AttrSuch s | s <- suchStmt]
   attrR       <- rule (attrROf lexicon term <|> attrSuch)
   attr        <- rule (attrOf lexicon term)
   verb        <- rule (verbOf lexicon sg term)
   fun         <- rule (funOf lexicon sg term)

   notionBase  <- rule (notionOf lexicon sg term)
   notionsBase <- rule (notionOf lexicon pl term)
   notion      <- rule [Notion as n ma | as <- many attrL, n <- notionBase, ma <- optional attrR]
   notions     <- rule [Notion as n ma | as <- many attrL, n <- notionsBase, ma <- optional attrR]

   notionVarBase <- rule (namedNominalOf lexicon term (math var))
   notionVar     <- rule [(Notion as n ma, x) | as <- many attrL, ~(n, x) <- notionVarBase, ma <- optional attrR]

   termExpr    <- rule [TermExpr f | f <- math formula]
   termFun     <- rule [TermFun p | _the, p <- fun]
   termIsolOp  <- rule [TermExpr (ExprConst f) | f <- math isolOp]
   term        <- rule (termExpr <|> termFun <|> termIsolOp)

-- Basic statements are statements without any conjunctions or quantifiers.
--
   stmtAttr    <- rule [StmtAttr t a | t <- term, _is, a <- attr]
   stmtAttrNeg <- rule [StmtNeg (StmtAttr t a) | t <- term, _is, _not, a <- attr]
   stmtVerb    <- rule [StmtVerb t x | t <- term, x <- verb]
   stmtNotion  <- rule [StmtNotion t n | t <- term, _is, _an, n <- notion]
   stmtExists  <- rule [StmtExists n | _exists, _an, n <- notion]
   stmtFormula <- rule [StmtFormula e | e <- math formula]
   stmtBase    <- rule (stmtAttr <|> stmtAttrNeg <|> stmtVerb <|> stmtNotion <|> stmtFormula <|> stmtExists)

   stmtOr  <- rule (stmtBase <|> [StmtConj Or s1 s2 | s1 <- stmtBase, _or, s2 <- stmt])
   stmtAnd <- rule (stmtOr   <|> [StmtConj And s1 s2 | s1 <- stmtOr, _and, s2 <- stmt])
   stmtIf  <- rule [StmtConj If s1 s2 | _if, s1 <- stmt, _then, s2 <- stmt]
   stmtNeg <- rule [StmtNeg s | _itIsWrong, s <- stmt]

   suchStmt <- rule [s | _suchThat, s <- stmt, optional _comma]

   all       <- rule [All xs Nothing b s | _all <|> _every, xs <- math vars, b <- optional suchStmt, optional _have, s <- stmt]
   allNotion <- rule [All xs (Just n) b s | _every, n <- notion, xs <- math vars, b <- optional suchStmt, optional _have, s <- stmt]

   some       <- rule [Some xs Nothing s | _exists, xs <- math vars, _suchThat, s <- stmt]
   someNotion <- rule [Some (pure x) (Just n) s | _exists, _an, ~(n, x) <- notionVar, _suchThat, s <- stmt]

   none        <- rule [None xs Nothing s | _exists, _no, xs <- math vars, _suchThat, s <- stmt]
   noneNotion  <- rule [None xs (Just n) s | _exists, _no, n <- notion, xs <- math vars, _suchThat, s <- stmt]

   stmtQuant       <- rule (all <|> some <|> none)
   stmtQuantNotion <- rule (allNotion <|> someNotion <|> noneNotion)

   stmt <- rule (stmtNeg <|> stmtIf <|> stmtQuant <|> stmtQuantNotion <|> stmtAnd)

   asmLetIn       <- rule [AsmLetIn xs e | _let, ~(xs, e) <- math typing]
   asmLetNotion   <- rule [AsmLetNom (pure x) n | _let, x <- math var, _be, _an, n <- notion]
   asmLetNotions  <- rule [AsmLetNom xs n | _let, xs <- math vars, _be, n <- notions]
   asmLetEq       <- rule [AsmLetEq x e | _let, ~(x, e) <- math assignment]
   asmLetThe      <- rule [AsmLetThe x f | _let, x <- math var, _be, _the, f <- fun]
   asmLet         <- rule (asmLetNotion <|> asmLetNotions <|> asmLetIn <|> asmLetEq <|> asmLetThe)
   asmSuppose     <- rule [AsmSuppose s | _suppose, s <- stmt]
   asm            <- rule ((asmLet <|> asmSuppose) <* _dot)

   axiom <- rule [Axiom as s | as <- many asm, optional _then, s <- stmt, _dot]

   thm <- rule [Thm as s | as <- many asm, optional _then, s <- stmt, _dot]

   defnAttr   <- rule [DefnAttr mn t a | mn <- optional (_an *> notion), t <- term, _is, a <- attr]
   defnVerb   <- rule [DefnVerb mn t v | mn <- optional (_an *> notion), t <- term, v <- verb]
   defnNotion <- rule [DefnNotion mn t n | mn <- optional (_an *> notion), t <- term, _is, _an, n <- notion]
   defnHead   <- rule (optional _write *> (defnAttr <|> defnVerb <|> defnNotion))
   defn       <- rule [Defn as head s | as <- many asm, head <- defnHead, _iff <|> _if, s <- stmt, _dot]

   -- In the future there needs to be dedicated functionality to handle isolated operators.
   -- For now we can just parse them as a bare command (assuming that theories get fresh notation).
   theoryHead   <- rule [(t, t', v) | _an, t <- notion, _extends, t' <- notion, v <- optional (math var)]
   theoryOps    <- rule $ math [[(f, ty)] | f <- cmd, _colon, ty <- formula]
   theoryAxioms <- rule ([[] | _dot] <|> [[a] | _satisfying, a <- stmt, _dot])
   theory       <- rule [Theory t t' v fs as | ~(t, t', v) <- theoryHead, _equipped, fs <- theoryOps, as <- theoryAxioms]

-- TODO Decide on reference format and implement this production rule.
--
   byRef <- rule (pure Nothing)

   proofAsm     <- rule [ProofAsm a | a <- asm]
   proofHave    <- rule [ProofHave ms s by | ms <- optional (_since *> stmt <* _comma), _have, s <- stmt, by <- byRef, _dot]
   proofFix     <- rule [ProofFix x e | _fix, ~(x, e) <- math typing, _dot]
   proofSubGoal <- rule [ProofSubGoal goal sub thus | _show, goal <- stmt, _dot, sub <- proof, _thus, thus <- stmt, _dot]
   proofStep    <- rule (proofAsm <|> proofSubGoal <|> proofFix <|> proofHave)
   proof        <- rule [Proof steps | steps <- many proofStep]

   instrLet  <- rule [InstrAsm a | optional _throughout, a <- asm]
-- instrUse  <- rule [InstrUse i | optional _throughout, a <- use]

   paraAxiom  <- rule [ParaAxiom tag p | ~(tag, p) <- env "axiom" axiom]
   paraThm    <- rule [ParaThm tag p | ~(tag, p) <- env "theorem" thm]
   paraProof  <- rule [ParaProof tag p | ~(tag, p) <- env "proof" proof]
   paraDefn   <- rule [ParaDefn p | p <- env_ "definition" defn]
   paraTheory <- rule [ParaTheory p | p <- env_ "theory" theory]
   para       <- rule (paraAxiom <|> paraThm <|> paraDefn <|> paraTheory <|> paraProof <|> instrLet)

-- Starting category.
--
   pure (many para)




-- This function could be rewritten, so that it can be used directly in the grammar,
-- instead of with specialized variants.
--
patternOf
   :: (pat -> [Term] -> b)
   -> Lexicon
   -> (Lexicon -> Set pat)
   -> (pat -> Pattern)
   -> Prod r e Tok Term
   -> Prod r e Tok b
patternOf constr lexicon selector proj arg =
   [constr pat args | ~(args, pat) <- asum (fmap make pats)]
   where
      pats = Set.toList (selector lexicon)
      make pat = [(args, pat) | args <- go (proj pat)]
      go = \case
         Just w : ws  -> [as | token w, as <- go ws]
         Nothing : ws -> [a:as | a <- arg, as <- go ws]
         []           -> pure []

attrLOf :: Lexicon -> Prod r e Tok Term -> Prod r e Tok AttrL
attrLOf lexicon arg = patternOf AttrL lexicon lexiconAttrLs id arg

attrROf :: Lexicon -> Prod r e Tok Term -> Prod r e Tok AttrR
attrROf lexicon arg = patternOf AttrR lexicon lexiconAttrRs id arg

attrOf :: Lexicon -> Prod r e Tok Term -> Prod r e Tok Attr
attrOf lexicon arg = patternOf Attr lexicon lexiconAttr id arg

notionOf :: Lexicon -> (SgPl Pattern -> Pattern) -> Prod r e Tok Term -> Prod r e Tok BaseNotion
notionOf lexicon proj arg = patternOf BaseNotion lexicon lexiconNoms proj arg

verbOf :: Lexicon -> (SgPl Pattern -> Pattern) -> Prod r e Tok Term -> Prod r e Tok Verb
verbOf lexicon proj arg = patternOf Verb lexicon lexiconVerbs proj arg

funOf :: Lexicon -> (SgPl Pattern -> Pattern) -> Prod r e Tok Term -> Prod r e Tok Fun
funOf lexicon proj arg = patternOf Fun lexicon lexiconFuns proj arg


-- A notion pattern with a variable as name. Uses `arg` for
-- the slots, and `var` for the name. The notion patterns are
-- obtained from `lexicon`.
--
namedNominalOf :: Lexicon -> Prod r e Tok Term -> Prod r e Tok var -> Prod r e Tok (BaseNotion, var)
namedNominalOf lexicon arg var =
   [(BaseNotion pat (args1 <> args2), x) | ~(args1, x, args2, pat) <- asum (fmap make pats)]
   where
      pats = Set.toList (lexiconNoms lexicon)
      make pat =
         let (pat1, pat2) = splitOnPreposition (sg pat)
         in  [(args1, x, args2, pat) | args1 <- go pat1, x <- var, args2 <- go pat2]
      go = \case
         Just w : ws  -> [as | token w, as <- go ws]
         Nothing : ws -> [a:as | a <- arg, as <- go ws]
         []           -> pure []




command :: Text -> Prod r e Tok Tok
command cmd = token (Command cmd)

begin, end :: Text -> Prod r e Tok Tok
begin kind = token (BeginEnv kind)
end kind   = token (EndEnv kind)

-- Surround a production rule `body` with an environment of a certain `kind`.
-- Skips optional names after the beginning of the environment and also
-- returns the content of a label, if present.
--
env :: Text -> Prod r e Tok a -> Prod r e Tok (Maybe Text, a)
env kind body = [(l, b) | begin kind, optional tag, l <- label, b <- body, end kind]
   where
      tag :: Prod r e Tok [Tok]
      tag = bracket (many (satisfy (/= Close Bracket)))

-- `env_` is like `env`, but without allowing labels.
--
env_ :: Text -> Prod r e Tok a -> Prod r e Tok a
env_ kind body = [b | begin kind, _ <- label, b <- body, end kind]

-- A label for referencing.
--
label :: Prod r e Tok (Maybe Text)
label = optional [mconcat ls | command "label", ls <- group (many (terminal maybeLabelTok))]

math :: Prod r String Tok a -> Prod r String Tok a
math body = [b | begin "math", b <- body, end "math"]

paren :: Prod r e Tok a -> Prod r e Tok a
paren body = [b | token (Open Paren), b <- body, token (Close Paren)]

bracket :: Prod r e Tok a -> Prod r e Tok a
bracket body = [b | token (Open Bracket), b <- body, token (Close Bracket)]

group :: Prod r e Tok a -> Prod r e Tok a
group body = [b | token (Open Invis), b <- body, token (Close Invis)]


word :: Text -> Prod r String Tok Tok
word w = token (Word w) <?> Text.unpack w

commaSep :: Prod r String Tok a -> Prod r String Tok (NonEmpty a)
commaSep item = [i:|is | i <- item, is <- many (_comma *> item)]


maybeVarTok :: Tok -> Maybe Var
maybeVarTok = \case
   Variable x -> Just (Var x)
   _tok -> Nothing

maybeWordTok :: Tok -> Maybe Text
maybeWordTok = \case
   Word n -> Just n
   _tok -> Nothing

maybeNumberTok :: Tok -> Maybe Text
maybeNumberTok = \case
   Number n -> Just n
   _tok -> Nothing

maybeCmdTok :: Tok -> Maybe Text
maybeCmdTok = \case
   Command n -> Just n
   _tok -> Nothing

-- Tokens that are allowed to appear in labels of environments.
--
maybeLabelTok :: Tok -> Maybe Text
maybeLabelTok = \case
   Symbol "'" ->Just "'"
   Symbol "-" -> Just ""
   _tok -> maybeWordTok _tok

maybeTagTok :: Tok -> Maybe Text
maybeTagTok = maybeLabelTok
