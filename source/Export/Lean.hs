{-# LANGUAGE DeriveAnyClass #-}

module Export.Lean where

import Base
import qualified Export.ContextGraph as CG
import Grammar.Abstract
import Grammar.Lexicon

import Control.Monad.State
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.List as List


data ExportState = ExportState
  { lemmaCount :: Word64
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

warn :: Text -> State ExportState ()
warn t = modify (\s -> s { warnings = warnings s ++ [t] })

export :: [Para] -> Lexicon -> Text
export doc regis = evalState exporting (ExportState 0 [] regis 0)
   where
   exporting :: State ExportState Text
   exporting = renderStrict <$> layoutSmart (LayoutOptions (AvailablePerLine 80 1)) <$> do
      doc' <- exportDocument doc
      pure $ vsep [pretty preamble,
                  line, doc']

exportDocument :: [Para] -> State ExportState (Doc a)
exportDocument decls = vsep <$> toList <$> traverse exportDeclaration decls

exportDeclaration :: Para -> State ExportState (Doc ann)
exportDeclaration = \case
   ParaAxiom label axiom -> exportAxiom label axiom
   ParaThm label thm -> exportTheorem label thm
   ParaProof label proof -> error "TODO"
   ParaDefn defn -> exportDefinition defn
   InstrAsm asm -> error "TOOD"
   InstrUse -> error "TODO"

getNameFromPattern :: Pattern -> State ExportState Text
getNameFromPattern = go False "" . reverse
  where
    go us t [] = pure t
    go us t (Nothing : ps) = go us t ps -- we render holes in a pattern as nothing special
    go us t (Just (Word p) : ps) = go True (add_ us p <> t) ps
    go us t (Just (Symbol p) : ps) = go False (p <> t) ps
    go us t (Just (Number p) : ps) = go False (p <> t) ps
    go us t (Just p : ps) = warn ("Ignored token in pattern: " <> Text.pack (show p)) >> go us t ps

    add_ True = (<> "_")
    add_ False = id

-- | Get the name of the singular from a pattern.
getNameFromPredicateHead :: DefnHead -> State ExportState Text
getNameFromPredicateHead = \case
  DefnAttr mn term (Attr pat terms) -> getNameFromPattern pat
  DefnVerb mn term (Verb (SgPl pat1 pat2) terms) -> getNameFromPattern pat1
  DefnNotion mn term (Notion ls (BaseNotion (SgPl pat1 pat2) terms) rm) -> getNameFromPattern pat1

data LeanType = LeanType Text [Lean]
  deriving (Eq, Show)

data LeanInfo = LeanInfo
  { type_ :: Maybe (Last LeanType)
  , constraints :: [Lean]
  } deriving (Eq, Show, Semigroup)

instance Monoid LeanInfo where
  mempty = LeanInfo Nothing []

data LeanVarKind = Implicit LeanInfo | Normal LeanInfo
   deriving (Eq, Show)

instance Semigroup LeanVarKind where
  Implicit m1 <> Implicit m2 = Implicit (m1 <> m2)
  Implicit m1 <> Normal m2 = Normal (m1 <> m2)
  Normal m1 <> Implicit m2 = Normal (m1 <> m2)
  Normal m1 <> Normal m2 = Normal (m1 <> m2)

instance Monoid LeanVarKind where
  mempty = Implicit mempty

data Lean = LVar Text LeanVarKind
  | LPrefixApp Lean Lean  -- LPrefixApp "f" "a"
  | LInfixApp Text Lean Lean -- LInfixApp "+" "a" "b"
  | LNumber Text
  | LSymbol Text -- we will just copy symbols into the Lean code for now
  | LAll (NonEmpty Text) Lean
  | LConj Conj Lean Lean
   deriving (Eq, Show)

extractExpr :: Expr -> State ExportState Lean
extractExpr (ExprVar (Var v)) = pure $ LVar v mempty
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

extractChain :: Chain -> State ExportState Lean
extractChain (ChainBase (e :| es)) = foldl1 (LConj And) <$> mapM extractExpr (e : es)
extractChain (ChainCons (e :| es) r c) = spread <$> (go r [e : es] c >>= conv)
  where
    go r vars (ChainBase (e :| es)) = pure (r, (e : es) : vars)
    go r vars (ChainCons (e :| es) r' c) = do
      when (r /= r') $ warn $ "Conflicting relators in chain: " <> Text.pack (show r) <> " and " <> Text.pack (show r') <> ". Aborting."
      go r ((e : es) : vars) c

    conv = mapM (mapM (mapM extractExpr))

    spread (r, []) = error "can't happen"
    spread (r, es) = 
      let f = case r of
            Word w -> foldl' LPrefixApp (LSymbol w)
            Symbol w -> foldl1 (LInfixApp w)
            Command w -> foldl' LPrefixApp (LSymbol w)
      in foldl1 (LConj And) $ map f $ foldl' (\l e -> [e':l' | l' <- l, e' <- e]) [[]] es

extractTerm :: Term -> State ExportState Lean
extractTerm (TermExpr e) = extractExpr e
extractTerm (TermFun _) = error "Fun has yet to be implemented"

-- | TODO: We currently ignore AttrSuch
extractNotion :: Notion -> State ExportState LeanInfo
extractNotion (Notion attrLs (BaseNotion (SgPl pat _) terms) mattrR) = do
  name <- getNameFromPattern pat
  name_args <- mapM extractTerm terms
  let type_ = LeanType name name_args
  let rs = maybeToList $ mattrR >>= \case
        AttrR p ts -> Just (p, ts)
        AttrSuch _ -> Nothing
  let ls = [(p, t) | (AttrL p t) <- attrLs]
  constraints <- for (ls ++ rs) $ \(p, ts) -> do
    name <- getNameFromPattern p
    args <- mapM extractTerm ts
    pure $ foldl' LPrefixApp (LSymbol name) args
  pure $ LeanInfo (Just (Last type_)) constraints

fromSimpleTerm :: Term -> Text
fromSimpleTerm (TermExpr (ExprVar (Var t))) = t
fromSimpleTerm (TermExpr (ExprChain (ChainBase (ExprVar (Var m) :| [])))) = m
fromSimpleTerm t = error $ "Ouch! This should not have happened. I expected a simple term but got: " ++ show t

varsInTerm :: Term -> [(Text, LeanVarKind)]
varsInTerm (TermFun _) = error "Fun has yet to be implemented"
varsInTerm (TermExpr e) = varsInExpr e
  where
    varsInExpr = \case 
      (ExprVar (Var t)) -> [(t, mempty)]
      (ExprConst _) -> []
      (ExprNumber _) -> []
      (ExprOp _ es) -> concatMap varsInExpr es
      (ExprParen e) -> varsInExpr e
      (ExprApp e1 e2) -> varsInExpr e1 <> varsInExpr e2
      (ExprChain ch) -> varsInChain ch

    varsInChain (ChainBase es) = concatMap varsInExpr es
    varsInChain (ChainCons es _ ch) = concatMap varsInExpr es <> varsInChain ch

toNormal :: LeanVarKind -> LeanVarKind
toNormal (Implicit v) = Normal v
toNormal (Normal v) = Normal v

-- | We can assume that the 'Term' will consist of only a variable.
varsInDefn :: DefnHead -> State ExportState [(Text, LeanVarKind)]
varsInDefn (DefnAttr mn t (Attr _ ts)) = do
  info <- fromMaybe mempty <$> traverse extractNotion mn
  let v = fromSimpleTerm t
  let vs = concatMap varsInTerm ts
  pure $ [(v, Normal info)] ++ map (fmap toNormal) vs
varsInDefn _ = error "TODO"

-- splitAssumptions :: [Assumption] -> ([Typing Var Typ], [Statement])
-- splitAssumptions = go ([], [])
  -- where
    -- go acc [] = acc
    -- go (ts, ss) ((AssumptionPretyping ns):xs) = go (ts ++ toList ns, ss) xs
    -- go (ts, ss) ((Assumption s):xs) = go (ts, ss ++ [s]) xs

-- mkVars :: [Typing Var Typ] -> [(Var, Maybe Typ)] -> [Var] -> State ExportState [(Var, LeanVar)]
-- mkVars ts vs frees = do
  -- let wrap v = (v, mempty)
  -- let m = id
        -- -- $ CG.addBag  (concatMap (\(_ `Inhabits` t) -> map wrap . Set.toList . freeVariables $ t) ts)
        -- -- $ CG.addBag  (concatMap (\(_, t) -> maybe [] (map wrap . Set.toList . freeVariables) t) vs)
        -- $ CG.addBag  (map wrap frees)
        -- $ CG.addList (map (\(v, t) -> (v, Normal (Last <$> t))) vs)
        -- $ CG.addBag  (map (\(v `Inhabits` t) -> (v, Implicit (Just (Last t)))) ts)
        -- $ CG.empty
  -- case CG.linearize m of
    -- Nothing -> warn "Cycles!" >> pure []
    -- Just xs -> pure $ xs

extractStmt :: Stmt -> State ExportState Lean
extractStmt (StmtFormula f) = extractExpr f
extractStmt (All vs mstmt stmt) = do
  asms <- mapM extractStmt $ maybeToList mstmt
  claim <- extractStmt stmt
  let body = case asms of
        [] -> claim
        asms -> LConj If (foldl1 (LConj And) asms) claim
  pure $ LAll (fmap unVar vs) body
extractStmt (StmtConj c s1 s2) = do
  s1' <- extractStmt s1
  s2' <- extractStmt s2
  pure $ LConj c s1' s2'
extractStmt _ = error "TODO"

extractAsm :: Asm -> State ExportState Lean
extractAsm = error "TODO"

exportLeanVars :: [(Text, LeanVarKind)] -> Doc ann
exportLeanVars lv = hsep $ map go lv
  where
    go (v, Normal (LeanInfo Nothing _)) = "(" <> pretty v <> ")"
    go (v, Normal (LeanInfo (Just t) _)) = "(" <> pretty v <> " : " <> showLastType t <> ")"
    go (v, Implicit (LeanInfo Nothing _)) = "{" <> pretty v <> "}"
    go (v, Implicit (LeanInfo (Just t) _)) = "{" <> pretty v <> " : " <> showLastType t <> "}"

    showLastType lt = let (LeanType t ts) = getLast lt
      in exportLean (foldl' LPrefixApp (LSymbol t) ts)

exportDefinition :: Defn -> State ExportState (Doc ann)
exportDefinition (Defn asms dhead stmt) = do
  name <- getNameFromPredicateHead dhead
  vars <- exportLeanVars <$> varsInDefn dhead
  asms' <- mapM extractAsm asms
  stmt' <- extractStmt stmt
  pure $ vsep [hsep ["def", pretty name, vars, " : Prop :="], indent 2 (exportLean stmt')]

-- exportSignature :: [(Var, Maybe Typ)] -> [Assumption] -> Statement -> Doc ann -> State ExportState (Doc ann)
-- exportSignature vars asms stmt result = do
  -- let (tp, ass) = splitAssumptions asms
  -- let assVars = Set.toList $ Set.unions $ map freeVariablesProp (stmt:ass)
  -- vs <- mkVars tp vars assVars
  -- r <- gets registry
  -- pure $ hsep $ [exportLeanVars r vs, ":", concatWith 
    -- (\a b -> a <> " -> " <> b) (map (exportProp r) ass ++ [result])]

exportAxiom :: Label -> Axiom -> State ExportState (Doc ann)
exportAxiom nameMay (Axiom asms stmt) = do
  name <- maybe ((\i -> "ax_" <> Text.pack (show i)) <$> newLemmaId) pure nameMay
  pure ""

exportTheorem :: Label -> Thm -> State ExportState (Doc ann)
exportTheorem nameMay (Thm asms stmt) = do
  name <- maybe ((\i -> "thm_" <> Text.pack (show i)) <$> newLemmaId) pure nameMay
  pure ""

exportConj :: Conj -> Doc ann
exportConj = \case
  If -> "→"
  And -> "∧"
  Or -> "∨"
  Iff -> "↔"

exportLean :: Lean -> Doc ann
exportLean = \case
  (LVar t _) -> pretty t -- TODO
  (LPrefixApp l1 l2) -> hsep [exportLean l1, exportLean l2]
  (LInfixApp t l1 l2) -> hsep ["(", exportLean l1, pretty t, exportLean l2, ")"]
  (LNumber t) -> pretty t
  (LSymbol t) -> pretty t
  (LAll vs l) -> hsep $ "∀" : List.intersperse ", ∀" (pretty <$> toList vs) ++ [",", exportLean l]
  (LConj c l1 l2) -> hsep ["(", exportLean l1, exportConj c, exportLean l2, ")"]

-- -- TODO(anton): We should have a monad here for keeping track of precedences
-- -- and prefix/infixr/infixl/..
-- -- Some of the precedences can be found in init/core.lean the rest is guessed
-- exportProp :: Registry -> Prop -> Doc ann
-- exportProp r = go 0
  -- where 
    -- go b = \case
      -- (Rel t `PredApp` x `PredApp` y) -> case lookupTok r t of
        -- Nothing -> prec b 100 $ viaShow t <+> exportExpr' 100 r x <+> exportExpr' 100 r y
        -- Just f -> go b (f x y)
      -- Falsum -> "false"
      -- Verum -> "true"
      -- Squashed e -> prec b 100 $ "nonempty" <+> exportExpr' 100 r e
      -- Predicate p -> pretty p
      -- Rel tok -> viaShow tok
      -- PredicatePattern pat -> pretty $ patAsText pat
      -- Not (e1 `Equals` e2) -> prec b 50 $ exportExpr' 50 r e1 <+> "≠" <+> exportExpr' 50 r e2
      -- Not p                -> prec b 40 $ "¬" <+> go 40 p
      -- p@(_ `PredApp` _) `PredApp` e -> prec b 100 $ go 99 p <+> exportExpr' 100 r e
      -- p `PredApp` e  -> prec b 100 $ go 100 p <+> exportExpr' 100 r e
      -- e1 `Equals` e2 -> prec b 50 $ exportExpr' 50 r e1 <+> "=" <+> exportExpr' 50 r e2
      -- p@(_ `And` _) `And` q -> prec b 35 $ go 34 p <+> "∧" <+> go 35 q
      -- p `And` q             -> prec b 35 $ go 35 p <+> "∧" <+> go 35 q
      -- p@(_ `Or` _) `Or` q   -> prec b 35 $ go 34 p <+> "∨" <+> go 35 q
      -- p `Or` q              -> prec b 35 $ go 35 p <+> "∨" <+> go 35 q
      -- p `Implies` q@(_ `Implies` _) -> prec b 20 $ go 19 p <+> "->" <+> go 20 q
      -- p `Implies` q         -> prec b 20 $ go 20 p <+> "->" <+> go 20 q
      -- Quantify quant v Hole p -> prec b 10 $ pretty quant <+> pretty v <> "," <+> go 9 p
      -- Quantify quant v ty p -> prec b 10 $
         -- pretty quant <+> pretty v <+> ":" <+> exportExpr' 5 r ty <> "," <+> go 9 p

-- exportExpr :: Registry -> Expr -> Doc ann
-- exportExpr = exportExpr' 0

-- exportExpr' :: Int -> Registry -> Expr -> Doc ann
-- exportExpr' i _ = go i
  -- where
    -- go b = \case
      -- Hole -> "_"
      -- Const c -> pretty c
      -- ConstPattern pat -> pretty $ patAsText pat
      -- Bottom -> "∅"
      -- Top -> "*"
      -- Free v -> pretty v
      -- e1 `Times` e2 -> prec b 35 $ go 35 e1 <+> "×" <+> go 35 e2
      -- e1 `Plus` e2  -> prec b 30 $ go 35 e1 <+> "⊕" <+> go 35 e2
      -- e1 `To` e2@(_ `To` _) -> prec b 20 $ go 19 e1 <+> "->" <+> go 20 e2
      -- e1 `To` e2    -> prec b 20 $ go 20 e1 <+> "->" <+> go 20 e2
      -- (Const "prim_mul" `App` e1) `App` e2  -> prec b  65 $ go 65 e1 <+> "*" <+> go 65 e2
      -- (Const "prim_add" `App` e1) `App` e2 -> prec b  70 $ go 70 e1 <+> "+" <+> go 70 e2
      -- e1@(_ `App` _) `App` e2               -> prec b 100 $ go 99 e1 <+> go 100 e2
      -- e1 `App` e2                           -> prec b 100 $ go 100 e1 <+> go 100 e2
      -- _ -> error "missing case in exportExpr"

preamble :: Text
preamble = Text.intercalate "\n"
   [ "-- BEGIN PREAMBLE"
   , ""
   -- Defines special notation for almost-universal quantification.
   -- In the future this should be expanded to a type class for more general use.
   , "definition almost_all_nat (p : ℕ  -> Prop) := ∃ l, ∀ n : ℕ, n ≤ l → p n"
   , "notation `∀∞` binders `, ` r:(scoped P, almost_all_nat P) := r"
   , ""
   , "mutual inductive even, odd"
   , "with even : ℕ → Prop"
   , "| even_zero : even 0"
   , "| even_succ : ∀ n, odd n → even (n + 1)"
   , "with odd : ℕ → Prop"
   , "| odd_succ : ∀ n, even n → odd (n + 1)"
   , ""
   , "notation `∄` binders `, ` r:(scoped P, ¬ ∃ n, P n) := r"
   , "notation `natural_number` := ℕ"
   , "def divides {α} := @has_dvd.dvd α"
   , "def neq {α} (a : α) (b) := a ≠ b"
   , ""
   , "axiom omitted {p : Prop} : p"
   , ""
   , "-- END PREAMBLE"
   ]