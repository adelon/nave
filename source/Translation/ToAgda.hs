{-# LANGUAGE FunctionalDependencies #-}


-- Direct translation to Agda.


-- TODO.
-- Decide between `Concrete` and `Abstract` variants of Agda's syntax.
--
-- `Concrete` requires desugaring by Agda and re-does some of the work
-- that we did when parsing to our AST (e.g.: operator prec).
-- On the upside, it corresponds directly to Agda's surface syntax.
--
-- `Abstract` needs a bit of extra information for some nodes and probably
-- also more work on our AST (e.g.: binding variables properly?). Could be
-- the more morally correct choice.


-- TODO
-- It should be possible to decorate the translated AST with appropriate
-- source ranged (from `Agda.Syntax.Position`) and get the correct ranges
-- from Agda's type checker. We can then use those ranges to show the
-- correct range in the controlled natural language input.


module Translation.ToAgda where


import Base
import Grammar.Abstract
import Lex (printTok)

import qualified Agda.Syntax.Concrete as AConc
import qualified Agda.Syntax.Position as APos

import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Text as Text


type TranslM a = Reader TranslContext a

data TranslContext = TranslContext
   { builtinOperators :: Map Operator (AConc.Expr -> AConc.Expr -> AConc.Expr) 
   , builtinRelators  :: Map Relator (AConc.Expr -> AConc.Expr -> AConc.Expr) 
   }
-- TODO
-- The translation environment provides
-- a `Map` of builtin definitions, mapping builtins
-- to Agda identifiers.
--
-- There may be some other use cases as well
-- (variable naming, managing scopes). The
-- `Reader` monad should suffice for now.


builtins :: TranslContext
builtins = TranslContext
   { builtinOperators = Map.fromList
      [ (mkOp "+", mkQNameOp "+")
      ]
   , builtinRelators = Map.fromList
      [ (Symbol "=", mkQNameOp "≈")
      ]
   }
   where
      mkOp s = [Nothing, Just (Symbol s), Nothing]


class ToAgda a c | a -> c where
   toAgda :: a -> TranslM c


instance ToAgda Expr AConc.Expr where
   toAgda = \case
      ExprVar (Var x) -> pure (AConc.Ident (mkQName x))

      ExprParen e -> do
         e' <- toAgda e
         pure (AConc.Paren APos.NoRange e')
      --
      -- Using `RawApp` for now, since the actual `App` requires more metadata.
      e1 `ExprApp` e2 -> do
         e1' <- toAgda e1
         e2' <- toAgda e2
         pure (AConc.RawApp APos.NoRange [e1', e2'])

      ExprChain ch -> toAgda ch

      _ -> error "toAgda incomplete"


instance ToAgda Chain AConc.Expr where
   toAgda = \case
      ChainBase (e :| []) -> toAgda e
      ChainCons e1s rel (ChainBase e2s) -> do
         e1s' <- forM e1s toAgda
         e2s' <- forM e2s toAgda
         pure (foldr1 mkConj [fromRelator rel e1 e2 | e1 <- toList e1s', e2 <- toList e2s'])
      ChainCons e1s rel ch@(ChainCons e2s _ _) -> do
         e1s' <- forM e1s toAgda
         e2s' <- forM e2s toAgda
         ch'  <- toAgda ch
         pure (mkConj (foldr1 mkConj [fromRelator rel e1 e2 | e1 <- toList e1s', e2 <- toList e2s']) ch')
      _ -> error "Malformed Chain: Multiple expressions without relator."


fromRelator :: Tok -> AConc.Expr -> AConc.Expr -> AConc.Expr
fromRelator rel e1 e2 = mkApp (AConc.Ident (mkQName (flattenPattern [Just rel]))) [e1, e2]


instance ToAgda AttrL AConc.Expr where
   toAgda (AttrL pat terms) = do
      fun  <- fromAttrPattern pat
      args <- forM terms toAgda
      pure (mkApp fun args)
      

instance ToAgda Term AConc.Expr where
   toAgda = \case
      TermExpr e -> toAgda e
      TermFun f -> toAgda f
      
      
instance ToAgda Fun AConc.Expr where
   toAgda (Fun (SgPl pat _) args) = do
      fun'  <- fromFunPattern pat
      args' <- forM args toAgda
      pure (mkApp fun' args') 


-- TODO: This does not produce idiomatic names.
-- TODO: The types of tokens that appear in the AST should be changed so that this function is total.
flattenPattern :: Pattern -> Text
flattenPattern pat = Text.concat (fmap (Text.toTitle . printComponent) pat)
   where
      printComponent Nothing = "?"
      printComponent (Just tok) = case tok of
         Command w -> "$" <> w
         Word w -> w
         Symbol "=" -> "$eq"
         Symbol s -> s
         _ -> impossible "Precondition failed in flattenPattern: wrong token type"
         

flattenWordPattern :: Pattern -> Text
flattenWordPattern pat = Text.concat (fmap (Text.toTitle . printTok) (catMaybes pat))


fromAttrPattern :: Pattern -> TranslM AConc.Expr
fromAttrPattern pat = pure (AConc.Ident (mkQName ("Attr" <> flattenWordPattern pat)))

fromNomPattern :: Pattern -> TranslM AConc.Expr
fromNomPattern pat = pure (AConc.Ident (mkQName ("Nom" <> flattenWordPattern pat)))

fromVerbPattern :: Pattern -> TranslM AConc.Expr
fromVerbPattern pat = pure (AConc.Ident (mkQName ("Nom" <> flattenWordPattern pat)))

fromFunPattern :: Pattern -> TranslM AConc.Expr
fromFunPattern pat = pure (AConc.Ident (mkQName ("Fun" <> flattenWordPattern pat)))


mkQName :: Text -> AConc.QName
mkQName x = AConc.QName (AConc.Name APos.NoRange AConc.InScope x')
   where
      x' = [AConc.Id (Text.unpack x)]
      
mkQNameOp :: Text -> AConc.Expr -> AConc.Expr -> AConc.Expr
mkQNameOp x e1 e2 = mkApp (AConc.Ident (AConc.QName (AConc.Name APos.NoRange AConc.InScope x'))) [e1, e2]
   where
      x' = [AConc.Hole, AConc.Id (Text.unpack x), AConc.Hole]


mkApp :: AConc.Expr -> [AConc.Expr] -> AConc.Expr
mkApp fun args = AConc.RawApp APos.NoRange (fun : args)

mkConj :: AConc.Expr -> AConc.Expr -> AConc.Expr
mkConj = mkQNameOp "∧" 
