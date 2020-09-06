{-# LANGUAGE FunctionalDependencies #-}


module Translation.ToIntermediate where
   
{-

import Base
import Grammar.Abstract (Conj(..))

import qualified Grammar.Abstract as Surf
import qualified Semantics.Intermediate as Intermed


class ToIntermed a c | a -> c where
   toIntermed :: a -> c


instance ToIntermed Surf.Stmt Intermed.Stmt where
   toIntermed = \case
      Surf.All xs mn Nothing s -> Intermed.All xs mn (toIntermed s)
      Surf.All xs mn (Just b) s -> Intermed.All xs mn (Intermed.StmtConj If (toIntermed b) (toIntermed s))
   
-}