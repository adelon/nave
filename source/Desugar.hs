{-# LANGUAGE FunctionalDependencies #-}

module Desugar where


import Base

import Grammar.Abstract
import qualified Grammar.Desugared as D


class Desugarable s d | s -> d where
   desugar :: s -> d
   desugar = todo "desugar incomplete"



instance Desugarable Para D.Para
