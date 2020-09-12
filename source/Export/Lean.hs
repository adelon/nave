{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

module Export.Lean where

import Base
import qualified Export.ContextGraph as CG
import Grammar.Abstract
import Grammar.Lexicon

import Control.Monad.State
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.List as List


data ExportState = ExportState
  { lemmaCount :: Word64
  , constantCount :: Int
  , warnings :: [Text]
  , lexicon :: Lexicon
  , idCount :: Int
  }

newLemmaId :: State ExportState Word64
newLemmaId = do
  i <- gets lemmaCount
  modify (\s -> s { lemmaCount = lemmaCount s + 1 })
  pure i

freshId :: State ExportState Int
freshId = do
  i <- gets idCount
  modify (\s -> s { idCount = idCount s + 1 })
  pure i

newConstantId :: State ExportState Int
newConstantId = do
  i <- gets constantCount
  modify (\s -> s { constantCount = constantCount s + 1})
  pure i

warn :: Text -> State ExportState ()
warn t = modify (\s -> s { warnings = warnings s ++ [t] })

export :: [Para] -> Lexicon -> Text
export doc regis = evalState exporting (ExportState 0 0 [] regis 0)
   where
   exporting :: State ExportState Text
   exporting = renderStrict <$> layoutSmart (LayoutOptions (AvailablePerLine 80 1)) <$> do
      doc' <- exportDocument doc
      generatedWarnings <- gets warnings
      let prettyWarnings = vsep $ map (pretty . ("-- " <>)) generatedWarnings
      pure $ vsep [prettyWarnings, pretty preamble, line, doc']

-- ========================================================================= --
-- Parse AST -> Lean AST
-- ========================================================================= --

-- | We concatenate all words, symbols and numbers in a pattern
-- split by underscores if necessary. We ignore holes.
getNameFromPattern :: Pattern -> State ExportState Text
getNameFromPattern = go False "" . reverse
  where
    go _ t [] = pure t
    go us t (Nothing : ps) = go us t ps
    go us t (Just (Word p) : ps) = go True (add_ us p <> t) ps
    go _ t (Just (Symbol p) : ps) = go False (p <> t) ps
    go _ t (Just (Number p) : ps) = go False (p <> t) ps
    go us t (Just p : ps) = warn ("Ignored token in pattern: " <> Text.pack (show p)) >> go us t ps

    add_ True = (<> "_")
    add_ False = id

-- | Get the name of the singular from a pattern.
getNameFromPredicateHead :: DefnHead -> State ExportState Text
getNameFromPredicateHead = \case
  DefnAttr _ _ (Attr pat _) -> getNameFromPattern pat
  DefnVerb _ _ (Verb (SgPl pat1 _) _) -> getNameFromPattern pat1
  DefnNotion _ _ (Notion _ (BaseNotion (SgPl pat1 _) _) _) -> getNameFromPattern pat1

data LeanType = LeanType Text [Lean]
  deriving (Eq, Show)

-- TODO: This should be a warning!
instance Semigroup LeanType where
  a <> b = throw (LDuplicateTypes (show a) (show b))

data LAssumption
  = LTyped Var LeanType
  | LConstraint Lean
  deriving (Eq, Show)

data Quant = QAll | QMost | QSome | QNone | QUniq
  deriving (Eq, Show)

data Lean = LVar Var
  | LPrefixApp Lean Lean  -- ^ LPrefixApp "f" "a"
  | LNot Lean
  | LInfixApp Text Lean Lean -- ^ LInfixApp "+" "a" "b"
  | LNumber Text
  | LSymbol Text -- ^ we will just copy symbols into the Lean code for now
  | LQuant Quant (NonEmpty Var) (Maybe LeanType) Lean
  | LConj Conj Lean Lean
  | LEq Lean Lean
   deriving (Eq, Show)

freeVars :: Set Var -> Set Var -> Lean -> Set Var
freeVars bound acc = \case
  LVar v -> if v `Set.member` bound || v `Set.member` acc then acc
    else Set.insert v acc
  LPrefixApp l1 l2 -> freeVars bound (freeVars bound acc l2) l1
  LNot l -> freeVars bound acc l
  LInfixApp _ l1 l2 -> freeVars bound (freeVars bound acc l2) l1
  LNumber _ -> acc
  LSymbol _ -> acc
  LQuant _ vs mt l ->
    let acc' = case mt of
          Just (LeanType _ ls) -> foldl' (freeVars bound) acc ls
          Nothing -> acc
    in freeVars (bound <> Set.fromList (toList vs)) acc' l
  LConj _ l1 l2 -> freeVars bound (freeVars bound acc l2) l1
  LEq l1 l2 -> freeVars bound (freeVars bound acc l2) l1

freeVariables :: [Lean] -> Set Var
freeVariables = foldl' (freeVars mempty) mempty

appendConstraints :: [Lean] -> Lean -> Lean
appendConstraints cs l = foldl' (flip (LConj If)) l cs

extractExpr :: Expr -> State ExportState Lean
extractExpr (ExprVar v) = pure $ LVar v
extractExpr (ExprConst t) = do
  t' <- getNameFromPattern [Just t]
  pure $ LSymbol t'
extractExpr (ExprNumber t) = pure $ LNumber t
extractExpr (ExprOp op es) = do
    op' <- getNameFromPattern op
    es' <- mapM extractExpr es
    pure $ foldl' LPrefixApp (LSymbol op') es'
extractExpr (ExprParen e) = extractExpr e
extractExpr (ExprApp e1 e2) = LPrefixApp <$> extractExpr e1 <*> extractExpr e2
extractExpr (ExprChain ch) = extractChain ch

-- TODO: This can be nicer.
extractChain :: Chain -> State ExportState Lean
extractChain (ChainBase (e :| es)) = foldl1 (LConj And) <$> mapM extractExpr (e : es)
extractChain chain@(ChainCons _ r _) = spread <$> (go [] chain >>= conv)
  where
    go vars (ChainBase (e :| es)) = pure $ (e : es) : vars
    go vars (ChainCons (e :| es) r' c) = do
      when (r /= r') $ warn $ 
        "Conflicting relators in chain: " <> Text.pack (show r) <> " and " <> Text.pack (show r') <> ". Aborting."
      go ((e : es) : vars) c

    conv = mapM (mapM extractExpr)

    spread [] = error "can't happen"
    spread es = 
      let f = case r of
            Word w -> foldl' LPrefixApp (LSymbol w)
            Symbol w -> foldl1 (LInfixApp w)
            Command w -> foldl' LPrefixApp (LSymbol w)
            _ -> throw $ LOuch $ "You used a chain with a weird relator '" ++ show r ++ "'."
      in foldl1 (LConj And) $ map f $ foldl' (\l e -> [e':l' | l' <- l, e' <- e]) [[]] es

extractTerm :: Term -> State ExportState Lean
extractTerm (TermExpr e) = extractExpr e
extractTerm (TermFun _) = throw LFunNotImplemented

extractNotion :: Notion -> State ExportState (LeanType, [Lean])
extractNotion (Notion attrLs (BaseNotion (SgPl pat _) terms) mattrR) = do
  name <- getNameFromPattern pat
  name_args <- mapM extractTerm terms
  let type_ = LeanType name name_args
  let rs = maybeToList $ mattrR >>= \case
        AttrR p ts -> Just (p, ts)
        AttrSuch _ -> Nothing
  st <- case mattrR of
    Just (AttrSuch s) -> (:[]) <$> extractStmt s
    _ -> pure []
  let ls = [(p, t) | (AttrL p t) <- attrLs]
  constraints <- for (ls ++ rs) $ \(p, ts) -> do
    name' <- getNameFromPattern p
    args <- mapM extractTerm ts
    pure $ foldl' LPrefixApp (LSymbol name') (args ++ name_args)
  pure $ (type_, st ++ constraints)

extractQuantStmt :: Quant -> NonEmpty Var -> Maybe Notion -> Maybe Stmt -> Stmt -> State ExportState Lean
extractQuantStmt q vs mn mstmt stmt = do
  asms <- mapM extractStmt $ maybeToList mstmt
  mayTypeCs <- mapM extractNotion mn
  claim <- extractStmt stmt
  let body = foldr (LConj If) claim asms
  pure $ LQuant q vs (fst <$> mayTypeCs) 
    $ appendConstraints (concat $ maybeToList $ snd <$> mayTypeCs) body

-- TODO: Handle attributes uniformely.
extractStmt :: Stmt -> State ExportState Lean
extractStmt (StmtFormula f) = extractExpr f
extractStmt (StmtConj c s1 s2) = do
  s1' <- extractStmt s1
  s2' <- extractStmt s2
  pure $ LConj c s1' s2'
extractStmt (All vs mn mstmt stmt) = extractQuantStmt QAll vs mn mstmt stmt
extractStmt (Most vs mn mstmt stmt) = extractQuantStmt QMost vs mn mstmt stmt
extractStmt (Some vs mn stmt) = extractQuantStmt QSome vs mn Nothing stmt
extractStmt (None vs mn stmt) = extractQuantStmt QNone vs mn Nothing stmt
extractStmt (Uniq vs mn stmt) = extractQuantStmt QUniq vs mn Nothing stmt
extractStmt (StmtNeg s) = LNot <$> extractStmt s
extractStmt (StmtAttr t (Attr p ts)) = do
  name <- getNameFromPattern p
  args <- mapM extractTerm ts
  t' <- extractTerm t
  pure $ foldl' LPrefixApp (LSymbol name) (args ++ [t'])
extractStmt _ = throw $ LTODO "extractStmt"

extractAsm :: Asm -> State ExportState [LAssumption]
extractAsm = \case
  AsmSuppose s -> do
    stmt <- extractStmt s
    pure $ [LConstraint stmt]
  AsmLetNom vs n -> do
    (t, asm) <- extractNotion n
    pure $ map (\v -> LTyped v t) (toList vs) ++ map LConstraint asm
  AsmLetIn _ _ -> throw LFunNotImplemented
  AsmLetThe _ _ -> throw LAsmLetInNotImplemented
  AsmLetEq v e -> do
    expr <- extractExpr e
    pure $ [LConstraint (LEq (LVar v) expr)]

fromSimpleTerm :: Term -> Var
fromSimpleTerm (TermExpr (ExprVar v)) = v
fromSimpleTerm (TermExpr (ExprChain (ChainBase (ExprVar v :| [])))) = v
fromSimpleTerm t = throw $ LOuch $ "This should not have happened. I expected a simple term but got: " ++ show t

varsInTerm :: Term -> [Var]
varsInTerm (TermFun _) = throw LFunNotImplemented
varsInTerm (TermExpr e') = varsInExpr e'
  where
    varsInExpr = \case 
      (ExprVar v) -> [v]
      (ExprConst _) -> []
      (ExprNumber _) -> []
      (ExprOp _ es) -> concatMap varsInExpr es
      (ExprParen e) -> varsInExpr e
      (ExprApp e1 e2) -> varsInExpr e1 <> varsInExpr e2
      (ExprChain ch) -> varsInChain ch

    varsInChain (ChainBase es) = concatMap varsInExpr es
    varsInChain (ChainCons es _ ch) = concatMap varsInExpr es <> varsInChain ch

-- | We can assume that the 'Term' will consist of only a variable.
varsInDefnHead :: DefnHead -> State ExportState [(Var, Maybe (LeanType, [Lean]))]
varsInDefnHead (DefnAttr mn t (Attr _ ts)) = do
  info <- mapM extractNotion mn
  let v = fromSimpleTerm t
  let vs = concatMap varsInTerm ts
  pure $ [(v, info)] ++ map ((,Nothing)) vs
varsInDefnHead _ = throw $ LTODO "varsInDefnHead"

-- | TODO: We currently don't use the edges of the graph.
mkVars :: [(Var, (VarKind, Maybe LeanType))] 
       -> State ExportState [(Var, (VarKind, Maybe LeanType))]
mkVars ts = do
  let m = id
        $ CG.addBag ts
        $ CG.empty
  case CG.linearize m of
    Nothing -> warn "Cycles!" >> pure []
    Just xs -> pure $ xs

-- ========================================================================= --
-- Exporting
-- ========================================================================= --

exportDocument :: [Para] -> State ExportState (Doc a)
exportDocument decls = (vsep . List.intersperse "") <$> toList 
  <$> traverse exportDeclaration decls

exportDeclaration :: Para -> State ExportState (Doc ann)
exportDeclaration = \case
   ParaAxiom label axiom -> exportAxiom label axiom
   ParaThm label thm -> exportTheorem label thm
   ParaProof _ _ -> warn "Proofs are not yet implemented" >> pure ""
   ParaTheory  _ -> warn "Theories are not yet implemented" >> pure ""
   ParaDefn defn -> exportDefinition defn
   InstrAsm asm -> exportAssumption asm
   InstrUse -> throw $ LTODO "InstrUse"

-- TODO: Should asmConstraints be rendered as variables?
-- TODO: Some free variable may have been bound by global assumptions.
exportDefinition :: Defn -> State ExportState (Doc ann)
exportDefinition (Defn asms dhead stmt) = do
  name <- getNameFromPredicateHead dhead
  vars <- varsInDefnHead dhead
  let typedVars = map (fmap ((Normal,) . fmap fst)) vars
  let varAsms = concatMap (concatMap snd . maybeToList . snd) vars
  asms' <- concat <$> mapM extractAsm asms
  let asmTyped = flip concatMap asms' $ \case
        LTyped v t -> [(v, (Normal, Just t))]
        _ -> []
  let asmConstraints = flip concatMap asms' $ \case
        LConstraint l -> [l]
        _ -> []
  stmt' <- extractStmt stmt
  let frees = Set.toList $ freeVariables $ stmt' : asmConstraints
  orderedVars <- mkVars (typedVars ++ asmTyped ++ map (,(Implicit, Nothing)) frees)
  pure $ vsep [hsep ["def", pretty name, exportLeanVars orderedVars, ": Prop :="], 
    indent 2 (exportLean (appendConstraints (asmConstraints ++ varAsms) stmt'))]

exportAxThm :: Doc ann -> Text -> Tag -> [Asm] -> Stmt -> Maybe (Doc ann)
            -> State ExportState (Doc ann)
exportAxThm long short nameMay asms stmt proof = do
  name <- maybe ((\i -> short <> Text.pack (show i)) <$> newLemmaId) pure
    =<< (mapM (getNameFromPattern . fmap Just) nameMay)
  asms' <- concat <$> mapM extractAsm asms
  let asmTyped = flip concatMap asms' $ \case
        LTyped v t -> [(v, (Normal, Just t))]
        _ -> []
  let asmConstraints = flip concatMap asms' $ \case
        LConstraint l -> [l]
        _ -> []
  stmt' <- extractStmt stmt
  let frees = Set.toList $ freeVariables $ stmt' : asmConstraints
  orderedVars <- mkVars (asmTyped ++ map (,(Implicit, Nothing)) frees)
  pure $ vsep $ [hsep [long, pretty name, exportLeanVars orderedVars], 
    indent 2 (":" <+> exportLean (appendConstraints (asmConstraints) stmt'))]
    ++ ((indent 2) <$> maybeToList proof)

exportAxiom :: Tag -> Axiom -> State ExportState (Doc ann)
exportAxiom nameMay (Axiom asms stmt) =
  exportAxThm "axiom" "ax_" nameMay asms stmt Nothing

exportTheorem :: Tag -> Thm -> State ExportState (Doc ann)
exportTheorem nameMay (Thm asms stmt) = do
  exportAxThm "theorem" "thm_" nameMay asms stmt (Just ":= sorry")

exportAssumption :: Asm -> State ExportState (Doc ann)
exportAssumption asm = do
  lasm <- extractAsm asm
  fmap vsep $ forM lasm $ \case
    LTyped (Var v) t -> pure $  "variable " <> pretty v <> " : " <> exportLeanType t
    LConstraint l -> do 
      i <- newConstantId
      pure $ "constant c_" <> pretty (show i) <> " : " <> exportLean l

data VarKind = Normal | Implicit
  deriving (Eq, Show)

instance Semigroup VarKind where
  Implicit <> a = a
  Normal <> _ = Normal

exportLeanVars :: [(Var, (VarKind, Maybe LeanType))] -> Doc ann
exportLeanVars lv = hsep $ map go lv
  where
    go (Var v, (Normal, Nothing)) = "(" <> pretty v <> ")"
    go (Var v, (Normal, Just t)) = "(" <> pretty v <> " : " <> exportLeanType t <> ")"
    go (Var v, (Implicit, Nothing)) = "{" <> pretty v <> "}"
    go (Var v, (Implicit, Just t)) = "{" <> pretty v <> " : " <> exportLeanType t <> "}"

exportConj :: Conj -> Doc ann
exportConj = \case
  If -> "→"
  And -> "∧"
  Or -> "∨"
  Iff -> "↔"

exportQuant :: Quant -> Doc ann
exportQuant = \case
  QAll -> "∀"
  QMost -> "∀∞"
  QSome -> "∃"
  QNone -> "¬∃"
  QUniq -> "∃!"

exportLeanType :: LeanType -> Doc ann
exportLeanType (LeanType t ts) = exportLean $ foldl' (LPrefixApp) (LSymbol t) ts

exportLean :: Lean -> Doc ann
exportLean = \case
  (LVar (Var t)) -> pretty t
  (LPrefixApp l1 l2) -> hsep [exportLean l1, exportLean l2]
  (LInfixApp t l1 l2) -> hsep ["(", exportLean l1, pretty t, exportLean l2, ")"]
  (LNumber t) -> pretty t
  (LSymbol t) -> pretty t
  (LQuant q vs mt l) -> hsep $ (exportQuant q) : ((pretty . unVar) <$> toList vs) 
    ++ (((":" <+>) . exportLeanType) <$> maybeToList mt) ++ [",", exportLean l]
  (LConj c l1 l2) -> hsep ["(", exportLean l1, exportConj c, exportLean l2, ")"]
  (LNot l) -> "¬" <> exportLean l
  (LEq l1 l2) -> exportLean l1 <+> "=" <+> exportLean l2

preamble :: Text
preamble = Text.intercalate "\n"
   -- Defines special notation for almost-universal quantification.
   -- In the future this should be expanded to a type class for more general use.
   [ "definition almost_all_nat (p : ℕ  -> Prop) := ∃ l, ∀ n : ℕ, n ≤ l → p n"
   , "notation `∀∞` binders `, ` r:(scoped P, almost_all_nat P) := r"
   , ""
   , "notation `natural_number` := ℕ"
   , "notation `rational_number` := ℕ"
   , "def divides {α} := @has_dvd.dvd α"
   , "def neq {α} (a : α) (b) := a ≠ b"
   ]

-- ========================================================================= --
-- Error messages
-- ========================================================================= --

data LException
  = LTODO String
  | LAsmLetInNotImplemented
  | LFunNotImplemented
  | LOuch String
  | LDuplicateTypes String String
  deriving (Eq, Show)

instance Exception LException where
  displayException = \case
    LTODO s -> "TODO: " <> s
    LAsmLetInNotImplemented -> 
        "The Lean backend does not support statements of the form $n\\in\\natural_number$\n"
      <> "since arbitrary set inclusions can not be represented well in type theory.\n"
      <> "Try using 'Let k be a natural number'-like syntax instead."
    LFunNotImplemented ->
        "The 'Fun' feature has not yet been implemented"
    LOuch t -> "Ouch! " <> t <> " Please open an issue here: github.com/adelon/nave"
    LDuplicateTypes a b -> "A variables was declared with two different types: " <> a <> " and " <> b