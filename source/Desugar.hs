{-# LANGUAGE FunctionalDependencies #-}

{-|
   This module factors out syntactic simplifications that are independent
   of the choice of foundations.
-}


module Desugar where


import Base
import Grammar.Abstract
import qualified Grammar.Desugared as Desugared

import Control.Monad.State



type Desugaring = State DesugaringState

data DesugaringState = DesugaringState
   { currentFresh :: Word64
   } deriving (Show, Eq, Ord)

desugar :: Desugarable a b => a -> b
desugar a = evalState (desugaring a) initialDesugaringState
   where
      initialDesugaringState = DesugaringState
         { currentFresh = 0
         }



class Desugarable a b | a -> b where
   desugaring :: a -> Desugaring b
   desugaring = todo "desugaring incomplete"



instance Desugarable Para Desugared.Para
