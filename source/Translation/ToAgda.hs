{-# LANGUAGE FunctionalDependencies #-}

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
      --
      -- Using `RawApp` for now, since the actual `App` requires more metadata.
      e1 `ExprApp` e2 -> do
         e1' <- toAgda e1
         e2' <- toAgda e2
         pure (AConc.RawApp APos.NoRange [e1', e2'])

      _ -> error "toAgda incomplete"


mkQName :: Text -> AConc.QName
mkQName x = AConc.QName (AConc.Name APos.NoRange AConc.InScope x')
   where
      x' = [AConc.Id (Text.unpack x)]
