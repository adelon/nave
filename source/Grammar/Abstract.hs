{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Data types for the abstract syntax tree and helper functions
-- for constructing the lexicon.
--
module Grammar.Abstract (module Grammar.Abstract, module Lex) where


import Base
import Lex (Tok(..), Delim(..))

import Text.Earley (Grammar, Prod, (<?>), fullParses, parser, rule, token, satisfy)
import Text.Earley.Mixfix (Holey)
import qualified Data.Text as Text



newtype Var = Var {unVar :: Text} deriving (Show, Eq, Ord)

-- Split data by grammatical number.
data SgPl a = SgPl {sg :: a, pl :: a} deriving (Show, Eq, Ord, Functor)


-- In the concrete grammar there is a distinction between expressions
-- and formulae, to handle precedences correctly. In the abstract grammar
-- this distinction is not needed. Thus `Formula = Expr`.

data Expr
   = ExprVar Var
   | ExprConst Tok
   | ExprNumber Text
   | ExprOp Operator [Expr]
   | ExprParen Expr
   | ExprApp Expr Expr
   | ExprChain Chain
   deriving (Show, Eq, Ord)

type Operator = Holey Tok

data Chain
   = ChainBase (NonEmpty Expr)
   | ChainCons (NonEmpty Expr) Relator Chain
   deriving (Show, Eq, Ord)

type Relator = Tok

type Formula = Expr

type Connective = Holey Tok



type Pattern = Holey Tok



type BaseNotion = BaseNotionOf Term
data BaseNotionOf a
   = BaseNotion (SgPl Pattern) [a]
   deriving (Show, Eq, Ord)


type Notion = NotionOf Term
data NotionOf a
   = Notion [AttrL] (BaseNotionOf a) (Maybe AttrR)
   deriving (Show, Eq, Ord)




-- Should supersede all old notions eventually:
-- ForTheL style notions that can bring their own names with them.
--
type BaseNotion' = BaseNotionOf' Term
data BaseNotionOf' a
   = BaseNotion' (SgPl Pattern) [Var] [a]
--               ^^^^^^^^^^^^^^ ^^^^^ ^^^
--               Lexical item   Names Arguments
--
   deriving (Show, Eq, Ord)
--
-- For example 'an integer n' would essentially be
-- `BaseNotion (unsafeReadPattern "integer[s]") [Var "n"] []`


type Notion' = NotionOf' Term
data NotionOf' a
   = Notion' [AttrL] (BaseNotionOf' a) (Maybe AttrR)
   deriving (Show, Eq, Ord)


-- Left attributives (`AttrL`) modify notions from the left side,
-- e.g. `even`, `continuous`, and `σ-finite`.
--
data AttrL
   = AttrL Pattern [Term]
   deriving (Show, Eq, Ord)

-- Right attributes consist of basic right attributes, e.g.
-- `divisible by ?`, or `of finite type`, and
-- such-that conditions.
--
data AttrR
   = AttrR Pattern [Term]
   | AttrSuch Stmt
   deriving (Show, Eq, Ord)

-- For parts of the AST where attributes are not used to modify notions and
-- the L/R distinction does not matter.
-- For example, when then are used together with a copula, e.g. `n is even`
data Attr
   = Attr Pattern [Term]
   deriving (Show, Eq, Ord)

data Verb
   = Verb (SgPl Pattern) [Term]
   deriving (Show, Eq, Ord)

data Fun
   = Fun (SgPl Pattern) [Term]
   deriving (Show, Eq, Ord)



data Term
   = TermExpr Expr
   | TermFun Fun
   | TermSetOf Notion
   deriving (Show, Eq, Ord)

data Conj = If | And | Or | Iff deriving (Show, Eq, Ord)

data Stmt
   = StmtFormula Formula
   | StmtNeg Stmt
   | StmtAttr Term Attr
   | StmtVerb Term Verb
   | StmtNotion Term Notion
   | StmtExists Notion
   | StmtConj Conj Stmt Stmt
--
-- A quantification binds a nonempty list of variables.
--
-- The `Maybe Notion` is an optional typing.
--
-- The `Maybe Stmt`s in universal quantifications are additional constraints
-- on the variable (such-that-constraints). They do not occur in existential
-- quantifications, since 'such that' is used differently there.
--
   | All  (NonEmpty Var) (Maybe Notion) (Maybe Stmt) Stmt
   | Most (NonEmpty Var) (Maybe Notion) (Maybe Stmt) Stmt
   | Some (NonEmpty Var) (Maybe Notion) Stmt
   | None (NonEmpty Var) (Maybe Notion) Stmt
   | Uniq (NonEmpty Var) (Maybe Notion) Stmt
--
-- Missing generalized bounded quantifications: for all k < n ...
--
   deriving (Show, Eq, Ord)


data Asm
   = AsmSuppose Stmt
   | AsmLetNom (NonEmpty Var) Notion -- 'let k be an integer'
   | AsmLetIn (NonEmpty Var) Formula -- 'let $k\in\integers$'
   | AsmLetThe Var Fun -- 'let $g$ be the derivative of $f$'
   | AsmLetEq Var Expr -- 'let $m = n + k$'
   deriving (Show, Eq)

data Axiom = Axiom [Asm] Stmt
   deriving (Show, Eq)

data Thm = Thm [Asm] Stmt
   deriving (Show, Eq)

-- The head of the definition describes the part before the `iff`,
-- i.e. the definiendum. The `Maybe Notion` corresponds to an optional
-- type annotation for the `Term` of the head. The last part of the head
-- is the pattern being defined. The `Term` and the pattern being defined
-- must be 'simple'. This is not enforced syntactically, but with a
-- separate wf-check. A `Term` is simple if it is an expression that consists
-- of only a variable. A pattern is simple if it is precisely a pattern of
-- simple terms. Refer also to `isWfDefn` and to the following example.
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
   = DefnAttr (Maybe Notion) Term Attr
   | DefnVerb (Maybe Notion) Term Verb
   | DefnNotion (Maybe Notion) Term Notion -- TODO Remove.
   | DefnBaseNotion BaseNotion Notion
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
   { theoryDefines :: Notion
   , theoryExtends :: Notion
   , theoryExtendsVar :: Maybe Var
   , theoryHas :: [(Text, Expr)] -- Text is the name of the command.
   , theorySatisfies :: [Stmt]
   } deriving (Show, Eq)


data Inductive
   = InductiveFin (NonEmpty Text)
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
   | InstrAsm Asm
   | InstrUse -- Import/opening
   deriving (Show, Eq)





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
