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
import Grammar.Lexicon (Lexicon(..), lexiconAttr, splitOnVariableSlot)

import Text.Earley (Grammar, Prod, (<?>), rule, satisfy, terminal, token)
import Text.Earley.Mixfix (mixfixExpression)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty





grammar :: Lexicon -> Grammar r (Prod r String Tok [Para])
grammar lexicon@Lexicon{..} = mdo
   let ops   = map ((map . first . map . fmap) token) lexiconOperators
   let conns = map ((map . first . map . fmap) token) lexiconConnectives

   iden     <- rule (satisfy (`Set.member` lexiconIdens)    <?> "identifier")
   isolOp   <- rule (satisfy (`Set.member` lexiconIsolOps)  <?> "isolated operator")
   number   <- rule (terminal maybeNumberTok                <?> "number")
   relator  <- rule (satisfy (`Set.member` lexiconRelators) <?> "relator")
   var      <- rule (terminal maybeVarTok                   <?> "variable")
   vars     <- rule (commaList var)
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
   exprs       <- rule (commaList expr)

   chainBase   <- rule [ChainBase es | es <- exprs]
   chainCons   <- rule [ChainCons es r c | es <- exprs, r <- relator, c <- chain]
   chain       <- rule (chainCons <|> chainBase)

   formulaBase <- rule [ExprChain c | c <- chain]
   formula     <- mixfixExpression conns formulaBase ExprOp

-- These are asymmetric formulae, in the sense that we only allow
-- variables on one side. They express judgements.
--
   assignment  <- rule [(x, e) | x <- var, _eq <|> _defeq, e <- formula]
   typing      <- rule [(xs, e) | xs <- vars, _in <|> _colon, e <- formula]


-- Patterns always allow full terms as arguments. If stricter conditions are
-- to be placed on the arguments, then they have to verified with a
-- well-formedness check, not grammatically. This simplifies the grammar and
-- lets us give more specific error messages, since it is no longer a generic
-- 'unexpected <token>' error.
--
   adjL       <- rule (adjLOf lexicon term)
   adjR       <- rule (adjROf lexicon term) -- Should only be used with notions!
   adj        <- rule (adjOf lexicon term)
   verb        <- rule (verbOf lexicon sg term)
   fun         <- rule (funOf lexicon sg term)

   -- A basic right attribute or a conjunction of them, followed by an optional that-does phrase.
   attrRThat   <- rule [AttrRThat v | _that, v <- verb]
   attrRThats  <- rule ([[a] | a <- attrRThat] <|> [[a,a'] | a <- attrRThat, _and, a' <- attrRThat] <|> pure [])
   attrRs      <- rule ([[a] | a <- adjR] <|> [[a,a'] | a <- adjR, _and, a' <- adjR] <|> pure [])
   attrRight   <- rule [as <> as' | as <- attrRs, as' <- attrRThats]

   predicateVerb <- rule [PredicateVerb p | p <- verb]
   predicateAdj  <- rule [PredicateAdj p | _is, p <- adj]
   predicate     <- rule (predicateVerb <|> predicateAdj)
   thatPredicate <- rule (_that *> predicate)

   notionName   <- rule (math (commaList_ var) <|> pure [])

   notionBase   <- rule (notionOf lexicon sg term notionName)
   notion       <- rule [Notion as n as' ms  | as <- many adjL, n <- notionBase, as' <- attrRight, ms <- optional suchStmt]
   notionsBase  <- rule (notionOf lexicon pl term notionName)
   notions      <- rule [Notion as n as' ms | as <- many adjL, n <- notionsBase, as' <- attrRight, ms <- optional suchStmt]

   termExpr    <- rule [TermExpr f | f <- math formula]
   termFun     <- rule [TermFun p | _the, p <- fun]
   termIsolOp  <- rule [TermExpr (ExprConst f) | f <- math isolOp]
   termSetOf   <- rule [TermSetOf n | _the, _setOf, optional _all, n <- notions]
   term        <- rule (termExpr <|> termFun <|> termIsolOp <|> termSetOf)

-- Basic statements are statements without any conjunctions or quantifiers.
--
   stmtAttr    <- rule [StmtAdj t a | t <- term, _is, a <- adj]
   stmtAttrNeg <- rule [StmtNeg (StmtAdj t a) | t <- term, _is, _not, a <- adj]
   stmtVerb    <- rule [StmtVerb t x | t <- term, x <- verb]
   stmtNotion  <- rule [StmtNotion t n | t <- term, _is, _an, n <- notion]
   stmtExists  <- rule [StmtExists n | _exists, _an, n <- notion]
   stmtFormula <- rule [StmtFormula e | e <- math formula]
   stmtBase    <- rule (stmtAttr <|> stmtAttrNeg <|> stmtVerb <|> stmtNotion <|> stmtFormula <|> stmtExists)

   stmtOr  <- rule (stmtBase <|> [StmtConj Or s1 s2 | s1 <- stmtBase, _or, s2 <- stmt])
   stmtAnd <- rule (stmtOr   <|> [StmtConj And s1 s2 | s1 <- stmtOr, _and, s2 <- stmt])
   stmtIff <- rule (stmtAnd  <|> [StmtConj Iff s1 s2 | s1 <- stmtAnd, _iff, s2 <- stmt])
   stmtIf  <- rule [StmtConj If s1 s2 | _if, s1 <- stmt, optional _comma, _then, s2 <- stmt]
   stmtNeg <- rule [StmtNeg s | _itIsWrong, s <- stmt]

   suchStmt <- rule [s | _suchThat, s <- stmt, optional _comma]

   all       <- rule [All xs Nothing b s | _all <|> _every, xs <- math vars, b <- optional suchStmt, optional _have, s <- stmt]
   allNotion <- rule [All xs (Just n) b s | _every, n <- notion, xs <- math vars, b <- optional suchStmt, optional _have, s <- stmt]

   some        <- rule [Some xs Nothing s | _exists <|> _exist, xs <- math vars, _suchThat, s <- stmt]
   someNotion  <- rule [SomeNotion nx s | _exists, _an, nx <- notion, _suchThat, s <- stmt]
   someNotions <- rule [SomeNotion nx s | _exist, _an, nx <- notions, _suchThat, s <- stmt]

   none        <- rule [None xs Nothing s | _exists, _no, xs <- math vars, _suchThat, s <- stmt]
   noneNotion  <- rule [None xs (Just n) s | _exists, _no, n <- notion, xs <- math vars, _suchThat, s <- stmt]

   stmtQuant        <- rule (all <|> some <|> none)
   stmtQuantNotion  <- rule (allNotion <|> someNotion <|> noneNotion)
   stmtQuantNotions <- rule someNotions

   stmt <- rule (stmtNeg <|> stmtIf <|> stmtQuant <|> stmtQuantNotion <|> stmtQuantNotions <|> stmtIff)

   asmLetIn       <- rule [AsmLetIn xs e | _let, ~(xs, e) <- math typing]
   asmLetNotion   <- rule [AsmLetNom (pure x) n | _let, x <- math var, _be <|> _denote, _an, n <- notion]
   asmLetNotions  <- rule [AsmLetNom xs n | _let, xs <- math vars, _be <|> _denote, n <- notions]
   asmLetEq       <- rule [AsmLetEq x e | _let, ~(x, e) <- math assignment]
   asmLetThe      <- rule [AsmLetThe x f | _let, x <- math var, _be, _the, f <- fun]
   asmLet         <- rule (asmLetNotion <|> asmLetNotions <|> asmLetIn <|> asmLetEq <|> asmLetThe)
   asmSuppose     <- rule [AsmSuppose s | _suppose, s <- stmt]
   asm            <- rule (assumptionList (asmLet <|> asmSuppose) <* _dot)
   asms           <- rule [concat as | as <- many asm]
   --   asm            <- rule ((asmLet <|> asmSuppose) <* _dot)

   axiom <- rule [Axiom as s | as <- asms, optional _then, s <- stmt, _dot]

   thm <- rule [Thm as s | as <- asms, optional _then, s <- stmt, _dot]

   defnAttr       <- rule [DefnAdj mn t a | mn <- optional (_an *> notion), t <- term, _is, a <- adj]
   defnVerb       <- rule [DefnVerb mn t v | mn <- optional (_an *> notion), t <- term, v <- verb]
   defnNotion     <- rule [DefnNotion mn t n | mn <- optional (_an *> notion), t <- term, _is, _an, n <- notion]
   defnNoun <- rule [DefnNoun n n' | _an, n <- notionBase, _is, _an, n' <- notion]
   defnHead       <- rule (optional _write *> (defnAttr <|> defnVerb <|> defnNotion <|> defnNoun))
   defnIf         <- rule [Defn as head s | as <- asms, head <- defnHead, _iff <|> _if, s <- stmt, _dot]

   defnFunSymb <- rule [f | _comma, f <- termExpr, _comma]
   defnFun     <- rule [DefnFun as f mf t | as <- asms, f <- termFun, mf <- optional defnFunSymb, _is, t <- term, _dot]

   defn        <- rule (defnIf <|> defnFun)

   -- In the future there needs to be dedicated functionality to handle isolated operators.
   -- For now we can just parse them as a bare command (assuming that theories get fresh notation).
   theoryHead   <- rule [(t, t', v) | _an, t <- notion, _extends, t' <- notion, v <- optional (math var)]
   theoryFun    <- rule $ math [(f, ty) | f <- cmd, _colon, ty <- formula]
   theoryRel    <- rule ([(r, ExprConst "REL") | _an, n <- arity, _relation, r <- math cmd]
                     <|> [(r, ExprConst "EndRel" `ExprApp` ExprVar a) | _an, _relation, r <- math cmd, _on, a <- math var])
   theorySig    <- rule [NonEmpty.toList fs | _equipped, fs <- signatureList (theoryFun <|> theoryRel)]
   theoryAxioms <- rule ([[] | _dot] <|> [[a] | _satisfying, a <- stmt, _dot])
   theory       <- rule [Theory t t' v fs as | ~(t, t', v) <- theoryHead, fs <- theorySig, as <- theoryAxioms]

   inductiveFin <- rule [ InductiveFin cs | _an, notionBase, _is, _oneOf, cs <- orList2 (math cmd), _dot]
   inductive    <- rule inductiveFin

   signatureAttr  <- rule [SignatureAdj x a | x <- math var, _can, _be, a <- adjOf lexicon (math var)]
   signature      <- rule (signatureAttr <* _dot)

-- TODO Decide on reference format and implement this production rule.
--
   byRef <- rule (pure Nothing)

   proofAsm     <- rule [ProofAsm a | a <- asm]
   proofHave    <- rule [ProofHave ms s by | ms <- optional (_since *> stmt <* _comma), _have, s <- stmt, by <- byRef, _dot]
   proofFix     <- rule [ProofFix x e | _fix, ~(x, e) <- math typing, _dot]
   proofSubGoal <- rule [ProofSubGoal goal sub thus | _show, goal <- stmt, _dot, sub <- proof, _thus, thus <- stmt, _dot]
   proofStep    <- rule (proofAsm <|> proofSubGoal <|> proofFix <|> proofHave)
   proof        <- rule [Proof steps | steps <- many proofStep]

   instrLet          <- rule [InstrAsm a | optional _throughout, a <- asmLet <|> asmSuppose, _dot]
-- instrLeanPrelude  <- rule [instrLeanPrelude lean | env_ "leanprelude"  ]
-- instrUse          <- rule [InstrUse i | optional _throughout, a <- use]

   paraAxiom  <- rule [ParaAxiom tag p | ~(tag, p) <- env "axiom" axiom]
   paraThm    <- rule [ParaThm tag p | ~(tag, p) <- env "theorem" thm]
   paraProof  <- rule [ParaProof tag p | ~(tag, p) <- env "proof" proof]
   paraDefn   <- rule [ParaDefn p | p <- env_ "definition" defn]
   paraTheory <- rule [ParaTheory p | p <- env_ "theory" theory]
   paraInd    <- rule [ParaInd p | p <- env_ "inductive" inductive]
   paraSig    <- rule [ParaSig p | p <- env_ "signature" signature]
   para       <- rule (paraAxiom <|> paraThm <|> paraDefn <|> paraTheory <|> paraInd <|> paraSig <|> paraProof <|> instrLet)

-- Starting category.
--
   pure (many para)




-- A disjunctive list with at least two items:
-- * 'a or b'
-- * 'a, b, or c'
-- * 'a, b, c, or d'
--
orList2 :: Prod r String Tok a -> Prod r String Tok (NonEmpty a)
orList2 item = [i:|is | i <- item, is <- many (_commaOr *> item)]
   <|> [i:|[j] | i <- item, _or, j <- item]


-- Nonempty textual lists of the form "a, b, c, and d".
-- The final comma is mandatory, 'and' is not.
-- Also allows "a and b". Should therefore be avoided in contexts where
-- a logical conjunction would also be possible.
-- Currently also allows additionals 'and's after each comma...
--
signatureList :: Prod r String Tok a -> Prod r String Tok (NonEmpty a)
signatureList item = [i:|is | i <- item, is <- many (_commaAnd *> item)]
   <|> [i:|[j] | i <- item, _and, j <- item]

commaList :: Prod r String Tok a -> Prod r String Tok (NonEmpty a)
commaList item = [i:|is | i <- item, is <- many (_comma *> item)]

commaList_ :: Prod r String Tok a -> Prod r String Tok [a]
commaList_ item = NonEmpty.toList <$> commaList item


assumptionList :: Prod r String Tok a -> Prod r String Tok [a]
assumptionList item = NonEmpty.toList <$> signatureList item





-- This function could be rewritten, so that it can be used directly in the grammar,
-- instead of with specialized variants.
--
patternOf
   :: (pat -> [a] -> b)
   -> Lexicon
   -> (Lexicon -> Set pat)
   -> (pat -> Pattern)
   -> Prod r e Tok a
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

adjLOf :: Lexicon -> Prod r e Tok arg -> Prod r e Tok (AdjLOf arg)
adjLOf lexicon arg = patternOf AdjL lexicon lexiconAdjLs id arg

adjROf :: Lexicon -> Prod r e Tok arg -> Prod r e Tok (AdjROf arg)
adjROf lexicon arg = patternOf AdjR lexicon lexiconAttrRs id arg

adjOf :: Lexicon -> Prod r e Tok arg -> Prod r e Tok (AdjOf arg)
adjOf lexicon arg = patternOf Adj lexicon lexiconAttr id arg

verbOf :: Lexicon -> (SgPl Pattern -> Pattern) -> Prod r e Tok Term -> Prod r e Tok Verb
verbOf lexicon proj arg = patternOf Verb lexicon lexiconVerbs proj arg

funOf :: Lexicon -> (SgPl Pattern -> Pattern) -> Prod r e Tok Term -> Prod r e Tok Fun
funOf lexicon proj arg = patternOf Fun lexicon lexiconFuns proj arg


-- A notion pattern with a variable as name. Uses `arg` for
-- the slots, and `var` for the name(s). The notion patterns are
-- obtained from `lexicon`.
--
notionOf :: Lexicon -> (SgPl Pattern -> Pattern) -> Prod r e Tok arg -> Prod r e Tok [Var] -> Prod r e Tok (NounOf arg)
notionOf lexicon proj arg vars =
   [Noun pat xs (args1 <> args2) | ~(args1, xs, args2, pat) <- asum (fmap make pats)]
   where
      pats = Set.toList (lexiconNoms lexicon)
      make pat =
         let (pat1, pat2) = splitOnVariableSlot (proj pat)
         in  [(args1, xs, args2, pat) | args1 <- go pat1, xs <- vars, args2 <- go pat2]
      go = \case
         Just w : ws  -> [as | token w, as <- go ws]
         Nothing : ws -> [a:as | a <- arg, as <- go ws]
         []           -> pure []





begin, end :: Text -> Prod r e Tok Tok
begin kind = token (BeginEnv kind)
end kind   = token (EndEnv kind)

-- Surround a production rule `body` with an environment of a certain `kind`.
-- Skips optional names after the beginning of the environment and also
-- returns the content of a label, if present.
--
env :: Text -> Prod r e Tok a -> Prod r e Tok (Maybe [Tok], a)
env kind body = [(t, b) | begin kind, t <- optional tag, optional label, b <- body, end kind]
   where
      tag :: Prod r e Tok [Tok]
      tag = bracket (many (satisfy (/= Close Bracket)))

-- `env_` is like `env`, but without allowing tags.
--
env_ :: Text -> Prod r e Tok a -> Prod r e Tok a
env_ kind body = [b | begin kind, optional label, b <- body, end kind]

-- A label for referencing.
--
label :: Prod r e Tok Text
label = [mconcat ls | command "label", ls <- group (many (terminal maybeLabelTok))]

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

command :: Text -> Prod r e Tok Tok
command cmd = token (Command cmd)


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
