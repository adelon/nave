{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Data types for the abstract syntax tree and helper functions
-- for constructing the lexicon.
--
module Grammar.Desugared (module Grammar.Desugared, module Grammar.Abstract, module Lex, module Grammar.Pattern) where


import Base
import Lex (Tok(..), Delim(..))
import Grammar.Pattern (Pattern, SgPl(..))


import Grammar.Abstract (Var(..), Chain(..), Formula, Expr(..), Term(..))
import Grammar.Abstract (AdjOf(..), Adj, VerbOf(..), Verb, NounOf(..), Noun, FunOf(..), Fun, Conj(..))
import qualified Grammar.Abstract as A





type NounPhrase = NounPhraseOf Term
data NounPhraseOf a
   = NounPhrase (NounOf a) [Stmt]
   deriving (Show, Eq, Ord)




type VerbPhrase = VerbPhraseOf Term
data VerbPhraseOf a
   = VPNot (VerbPhraseOf a)
   | VPVerb (VerbOf a)
   | VPAdj (AdjOf a)
   deriving (Show, Eq, Ord)



data Stmt
   = StmtExpr Expr
   | StmtNeg Stmt
   | StmtVerbPhrase Term VerbPhrase
   | StmtNounPhrase Term NounPhrase
   | StmtExists NounPhrase
   | StmtConj Conj Stmt Stmt
--
-- A quantification binds a nonempty list of variables.
--
-- The `Maybe NounPhrase` is an optional typing.
--
-- The `Maybe Stmt`s in universal quantifications are additional constraints
-- on the variable (such-that-constraints). They do not occur in existential
-- quantifications, since 'such that' is used differently there.
--
   | All  (NonEmpty Var) (Maybe NounPhrase) (Maybe Stmt) Stmt
   | Most (NonEmpty Var) (Maybe NounPhrase) (Maybe Stmt) Stmt
   | Some (NonEmpty Var) (Maybe NounPhrase) Stmt
   | SomeNounPhrase NounPhrase Stmt
   | None (NonEmpty Var) (Maybe NounPhrase) Stmt
   | Uniq (NonEmpty Var) (Maybe NounPhrase) Stmt
--
-- Missing generalized bounded quantifications: for all k < n ...
--
   deriving (Show, Eq, Ord)


data Asm
   = AsmSuppose Stmt
   | AsmLetNom (NonEmpty Var) NounPhrase -- 'let k be an integer'
   | AsmLetIn (NonEmpty Var) Expr -- 'let $k\in\integers$'
   | AsmLetThe Var Fun -- 'let $g$ be the derivative of $f$'
   | AsmLetEq Var Expr -- 'let $m = n + k$'
   deriving (Show, Eq)

data Axiom = Axiom [Asm] Stmt
   deriving (Show, Eq)

data Thm = Thm [Asm] Stmt
   deriving (Show, Eq)

-- The head of the definition describes the part before the `iff`,
-- i.e. the definiendum. The `Maybe NounPhrase` corresponds to an optional
-- type annotation for the `Term` of the head. The last part of the head
-- is the pattern being defined. The `Term` and the pattern being defined
-- must be 'simple'. This is not enforced syntactically, but with a
-- separate wf-check. A `Term` is simple if it is an expression that consists
-- of only a variable. A pattern is simple if it is precisely a pattern of
-- simple terms. Refer also to `isWfDefn` and to the following example.
-- This should eventually be superseded by a stricter grammar.
--
--   "A natural number   $n$        divides $m$   iff   ..."
--    ^^^^^^^^^^^^^^^^   ^^^        ^^^^^^^^^^^         ^^^
--    type annotation    term       verb                definiens
--    (a notion)         (simple)   (simple pattern)    (a statement)
--
--
--
-- A `DefnFun` consists of the functional notion (which must start with "the")
-- and an optional specification of a symbolic equivalent. The symbolic equivalent
-- does not need to have the same variables as the full functional notion pattern.
--
--   "The tensor product of $U$ and $V$ over $K$, $U\tensor V$, is ..."
--    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^     ^^^
--    definiendum                                 symbolic eqv.    definiens
--    (a functional notion)                       (an exression)   (a term)
--
data DefnHead
   = DefnAdj (Maybe NounPhrase) Term Adj
   | DefnVerb (Maybe NounPhrase) Term Verb
   | DefnNoun Noun NounPhrase
   deriving (Show, Eq)

data Defn
   = Defn [Asm] DefnHead Stmt
   | DefnFun [Asm] Term (Maybe Term) Term
   deriving (Show, Eq)

-- Well-formedness check for definitions.
-- The following conditions need to be met.
--
-- * Variables occurring in the patterns on the left side must be linear,
--   i.e. each variable can only occur once.
-- * The arguments of the patterns must be variables, not complex terms.
-- * The optional typing notion may not have any free variables.
-- * The rhs side may not have any free variables not occurring on the lhs.
-- * If a variable on the lhs does not occur on the rhs, a warning should we issued.
--
isWfDefn :: Defn -> Either DefnError Defn
isWfDefn = undefined
   where
      _areLinear vs = nubOrd vs == vs

data DefnError
   = DefnErrorLhsNotLinear
   | DefnErrorLhsComplexArgs
   | DefnErrorLhsTypeFree
   | DefnErrorRhsFree
   | DefnWarnLhsFree
   deriving (Show, Eq)

data ProofStep
   = ProofAsm [Asm]
   | ProofFix (NonEmpty Var) Expr
   --
   -- 'since <stmt>, we have <stmt> by <ref>'
   | ProofHave (Maybe Stmt) Stmt (Maybe Text)
   --
   -- 'show <goal stmt>. <steps>. thus <sufficient stmt>.'
   | ProofSubGoal Stmt Proof Stmt
   deriving (Show, Eq)



data Theory = Theory
   { theoryDefines :: NounPhrase
   , theoryExtends :: NounPhrase
   , theoryExtendsVar :: Maybe Var
   , theoryHas :: [(Text, Expr)] -- Text is the name of the command.
   , theorySatisfies :: [Stmt]
   } deriving (Show, Eq)


data Inductive
   = InductiveFin (NonEmpty Text)
   deriving (Show, Eq)


data Signature
   = SignatureAdj Var (AdjOf Var)
   deriving (Show, Eq)


data Proof
   = Proof [ProofStep]
   deriving (Show, Eq)

-- Optional tag: '\begin{<env>}[<tag>]'.
type Tag = Maybe [Tok]

data Para
   = ParaAxiom Tag Axiom
   | ParaThm Tag Thm
   | ParaProof Tag Proof
   | ParaDefn Defn
   | ParaTheory Theory
   | ParaInd Inductive
   | ParaSig Signature
   | InstrAsm Asm
   | InstrUse -- Import/opening
   deriving (Show, Eq)
