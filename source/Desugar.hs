{-# LANGUAGE FunctionalDependencies #-}

{-|
   This module factors out syntactic simplifications that are independent
   of the choice of foundations.
-}


module Desugar where


import Base

import Grammar.Abstract
import qualified Grammar.Desugared as D


type Desugaring = State DesugaringState

data DesugaringState = DesugaringState
   { currentFresh :: Word64
   } deriving (Show, Eq, Ord)

desguar :: Desugaring a
desugar = runState initialDesugaringState
   where
      initialDesugaringState = DesugaringState
         { currentFresh = 0
         }



class Desugarable s d | s -> d where
   desugaring :: s -> d
   desugaring = todo "desugar incomplete"



instance Desugarable Para D.Para
