{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS -Wno-name-shadowing #-}

module Export.ContextGraph where

import Base
import Grammar.Abstract (Var)

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

data ContextGraph a = ContextGraph
  { values :: IntMap.IntMap (Var, a)
  , indices :: Map.Map Var Int
  , preds :: IntMap.IntMap IntSet.IntSet
  , succs :: IntMap.IntMap IntSet.IntSet
  , fresh :: Int
  } deriving (Eq, Ord, Show)

addNode :: Semigroup a => Var -> a -> ContextGraph a -> ContextGraph a
addNode v t g@(ContextGraph values indices preds succs f) = case Map.lookup v indices of
  Nothing -> g 
    { indices = Map.insert v f indices
    , values = IntMap.insert f (v, t) values
    , preds = IntMap.insert f mempty preds
    , succs = IntMap.insert f mempty succs
    , fresh = f + 1 }
  Just i -> g { values = IntMap.update (\(_, t') -> Just (v, t' <> t)) i values}

addEdgeMay :: Var -> Var -> ContextGraph a -> Maybe (ContextGraph a)
addEdgeMay from to g = case (Map.lookup from (indices g), Map.lookup to (indices g)) of
  (Just fi, Just ti) -> Just $ g
    { preds = IntMap.update (\s -> Just $ IntSet.insert fi s) ti (preds g)
    , succs = IntMap.update (\s -> Just $ IntSet.insert ti s) fi (succs g)
    }
  _ -> Nothing

addEdge :: Monoid a => Var -> Var -> ContextGraph a -> ContextGraph a
addEdge from to g = fromJust $ addEdgeMay from to 
  $ addNode from mempty $ addNode to mempty $ g

addList :: Semigroup a => [(Var, a)] -> ContextGraph a -> ContextGraph a
addList [] g = g
addList ((v, a):xs) g = go (addNode v a g) v xs
  where
    go g _ [] = g
    go g last ((w, a):xs) = go (fromJust $ addEdgeMay last w $ addNode w a g) w xs

addBag :: Semigroup a => [(Var, a)] -> ContextGraph a -> ContextGraph a
addBag vs g = foldl' (\m (v, a) -> addNode v a m) g vs

addSet :: Semigroup a => Set.Set (Var, a) -> ContextGraph a -> ContextGraph a
addSet vs g = addBag (Set.toList vs) g

empty :: ContextGraph a
empty = ContextGraph
  { values = mempty
  , indices = mempty
  , preds = mempty
  , succs = mempty
  , fresh = 0
  }

-- | A digraph is acyclic iff it can be turned into a linear order (e.g. a list)
-- If the context graph is acyclic, it will compute such a linear order
-- using Kahn's algorithm; O(V + E), see Topological sorting on Wikipedia
-- TODO(anton): Return a cycle if the graph is cyclic.
linearize :: ContextGraph a -> Maybe [(Var, a)]
linearize (ContextGraph values _ preds succs _) = 
  map (\i -> values IntMap.! i) <$> go [] noIncomingEdges preds
  where
    noIncomingEdges = IntMap.foldlWithKey (\a k b -> if IntSet.null b then IntSet.insert k a else a) mempty preds

    go :: [Int] -> IntSet.IntSet -> IntMap.IntMap IntSet.IntSet -> Maybe [Int]
    go l s p = case IntSet.maxView s of
      Nothing -> if IntSet.null $ fold p then Just l else Nothing
      Just (n, s) -> 
        let ms = succs IntMap.! n
            p' = IntSet.foldl (\p'' m -> IntMap.update (Just . IntSet.delete n) m p'') p ms
            s' = IntSet.filter (\m -> IntSet.null (p' IntMap.! m)) ms
        in go (l ++ [n]) (IntSet.union s s') p'