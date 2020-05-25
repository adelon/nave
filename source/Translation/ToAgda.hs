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

import qualified Agda.Syntax.Concrete as AConc
import qualified Agda.Syntax.Position as APos

import Control.Monad.Reader

import qualified Data.Text as Text


type TranslM a = Reader TranslContext a

data TranslContext = TranslContext
-- TODO
-- The translation environment provides
-- a `Map` of builtin definitions, mapping builtins
-- to Agda identifiers.
--
-- There may be some other use cases as well
-- (variable naming, managing scopes). The
-- `Reader` monad should suffice for now.

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
      ChainBase es -> do
         es' <- forM es toAgda
         pure (foldr1 conj es')
         where
            conj e1 e2 = error "conj in toAgda undefined"
      ChainCons es rel ch -> error "toAgda incomplete"


mkQName :: Text -> AConc.QName
mkQName x = AConc.QName (AConc.Name APos.NoRange AConc.InScope x')
   where
      x' = [AConc.Id (Text.unpack x)]
