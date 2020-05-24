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
   exporting = renderStrict <$> layoutCompact <$> do
      doc' <- exportDocument doc
      pure $ vsep [pretty preamble, line, doc']

exportDocument :: [Para] -> State ExportState (Doc a)
exportDocument decls = vsep <$> toList <$> traverse exportDeclaration decls

exportDeclaration :: Para -> State ExportState (Doc ann)
exportDeclaration = \case
   ParaAxiom label axiom -> exportAxiom label axiom
   ParaThm label thm -> exportTheorem label thm
   ParaProof label proof -> pure ""
   ParaDefn defn -> exportDefinition defn
   InstrAsm asm -> pure ""
   InstrUse -> pure "" -- TODO: Imports

-- patAsText :: Pattern -> Text
-- patAsText = Text.intercalate "_" . filter (/="") . map (\case Word w -> w; Slot -> "")

getNameFromPredicateHead :: DefnHead -> Text
getNameFromPredicateHead = \case
  DefnAttr mn term (Attr pat terms) -> ""
  DefnVerb mn term (Verb (SgPl pat1 pat2) terms) -> ""
  DefnNotion mn term (Notion ls (BaseNotion (SgPl pat1 pat2) terms) rm) -> ""

-- splitAssumptions :: [Assumption] -> ([Typing Var Typ], [Statement])
-- splitAssumptions = go ([], [])
  -- where
    -- go acc [] = acc
    -- go (ts, ss) ((AssumptionPretyping ns):xs) = go (ts ++ toList ns, ss) xs
    -- go (ts, ss) ((Assumption s):xs) = go (ts, ss ++ [s]) xs

-- data LeanVar = Implicit (Maybe (Last Typ)) | Normal (Maybe (Last Typ))

-- instance Semigroup LeanVar where
  -- Implicit m1 <> Implicit m2 = Implicit (m1 <> m2)
  -- Implicit m1 <> Normal m2 = Normal (m1 <> m2)
  -- Normal m1 <> Implicit m2 = Normal (m1 <> m2)
  -- Normal m1 <> Normal m2 = Normal (m1 <> m2)

-- instance Monoid LeanVar where
  -- mempty = Implicit Nothing

-- varsInPatt :: PredicateHead -> [(Var, Maybe Typ)]
-- varsInPatt = \case
   -- PredicateAdjPattern vs _ -> toList vs
   -- PredicateVerbPattern vs _ -> toList vs
   -- PredicateNominalPattern vs _ -> toList vs
   -- PredicateRelator (v1, _, v2) -> [(v1, Nothing), (v2, Nothing)]

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

-- exportLeanVars :: Lexicon -> [(Var, LeanVar)] -> Doc ann
-- exportLeanVars r lv = hsep $ map go lv
  -- where
    -- go (v, Normal Nothing) = "(" <> pretty v <> ")"
    -- go (v, Normal (Just t)) = "(" <> pretty v <> " : " <> exportExpr r (getLast t) <> ")"
    -- go (v, Implicit Nothing) = "{" <> pretty v <> "}"
    -- go (v, Implicit (Just t)) = "{" <> pretty v <> " : " <> exportExpr r (getLast t) <> "}"

exportDefinition :: Defn -> State ExportState (Doc ann)
exportDefinition (Defn asms dhead stmt) = do
  let name = getNameFromPredicateHead dhead
  pure ""
  -- sig <- exportSignature (varsInPatt ph) asms stmt "Prop"
  -- r <- gets registry
  -- pure $ hsep $ ["def", pretty name, sig, ":=", exportProp r stmt]

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
  -- r <- gets registry
  -- sig <- exportSignature [] asms stmt (exportProp r stmt)
  -- pure $ hsep $ ["axiom", pretty name, sig]

exportTheorem :: Label -> Thm -> State ExportState (Doc ann)
exportTheorem nameMay (Thm asms stmt) = do
  name <- maybe ((\i -> "thm_" <> Text.pack (show i)) <$> newLemmaId) pure nameMay
  pure ""
  -- r <- gets registry
  -- sig <- exportSignature [] asms stmt (exportProp r stmt)
  -- pure $ hsep $ ["theorem", pretty name, sig, ":=", "omitted"]

-- lookupTok :: Registry -> Tok -> Maybe (Expr -> Expr -> Prop)
-- lookupTok reg tok = Map.lookup tok (relators reg) >>= \case
  -- "eq" -> Just $ \x -> \y -> x `Equals` y
  -- "ne" -> Just $ \x -> \y -> Not (x `Equals` y)
  -- t -> Just $ \x -> \y -> Predicate t `PredApp` x `PredApp` y

-- prec :: Int -> Int -> Doc a -> Doc a
-- prec ctx here d = if ctx >= here then "(" <> d <> ")" else d

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
   , "import data.nat.basic"
   , "import data.nat.dist"
   , "import data.rat"
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
   , "notation `rational_number` := ℚ"
   , ""
   , "axiom omitted {p : Prop} : p"
   , ""
   , "-- END PREAMBLE"
   ]