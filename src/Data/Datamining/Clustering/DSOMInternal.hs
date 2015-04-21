------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.DSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @DSOM@ internals. Most developers should
-- use @DSOM@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses #-}

module Data.Datamining.Clustering.DSOMInternal where

import qualified Data.Foldable as F (Foldable, foldr)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Math.Geometry.Grid as G (Grid(..), FiniteGrid(..))
import qualified Math.Geometry.GridMap as GM (GridMap(..))
import Data.Datamining.Clustering.Classifier(Classifier(..))
import Prelude hiding (lookup)

-- | A Self-Organising Map (DSOM).
--
--   Although @DSOM@ implements @GridMap@, most users will only need the
--   interface provided by @Data.Datamining.Clustering.Classifier@. If
--   you chose to use the @GridMap@ functions, please note:
--
--   1. The functions @adjust@, and @adjustWithKey@ do not increment the
--      counter. You can do so manually with @incrementCounter@.
--
--   2. The functions @map@ and @mapWithKey@ are not implemented (they
--      just return an @error@). It would be problematic to implement
--      them because the input DSOM and the output DSOM would have to
--      have the same @Metric@ type.
data DSOM gm x k p = DSOM
  {
    -- | Maps patterns to tiles in a regular grid.
    --   In the context of a SOM, the tiles are called "nodes"
    gridMap :: gm p,
    -- | A function which determines the how quickly the SOM learns.
    learningRate :: (x -> x -> x -> x),
    -- | A function which compares two patterns and returns a 
    --   /non-negative/ number representing how different the patterns
    --   are.
    --   A result of @0@ indicates that the patterns are identical.
    difference :: p -> p -> x,
    -- | A function which updates models.
    --   If this function is @f@, then @f target amount pattern@ returns
    --   a modified copy of @pattern@ that is more similar to @target@
    --   than @pattern@ is.
    --   The magnitude of the adjustment is controlled by the @amount@
    --   parameter, which should be a number between 0 and 1.
    --   Larger values for @amount@ permit greater adjustments.
    --   If @amount@=1, the result should be identical to the @target@.
    --   If @amount@=0, the result should be the unmodified @pattern@.
    makeSimilar :: p -> x -> p -> p
  }

instance (F.Foldable gm) => F.Foldable (DSOM gm x k) where
  foldr f x g = F.foldr f x (gridMap g)

instance (G.Grid (gm p)) => G.Grid (DSOM gm x k p) where
  type Index (DSOM gm x k p) = G.Index (gm p)
  type Direction (DSOM gm x k p) = G.Direction (gm p)
  indices = G.indices . gridMap
  distance = G.distance . gridMap
  neighbours = G.neighbours . gridMap
  contains = G.contains . gridMap
  viewpoint = G.viewpoint . gridMap
  directionTo = G.directionTo . gridMap
  tileCount = G.tileCount . gridMap
  null = G.null . gridMap
  nonNull = G.nonNull . gridMap

instance
  (F.Foldable gm, GM.GridMap gm p, G.FiniteGrid (GM.BaseGrid gm p)) =>
    GM.GridMap (DSOM gm x k) p where
  type BaseGrid (DSOM gm x k) p = GM.BaseGrid gm p
  toGrid = GM.toGrid . gridMap
  toMap = GM.toMap . gridMap
  mapWithKey = error "Not implemented"
  delete k = withGridMap (GM.delete k)
  adjustWithKey f k = withGridMap (GM.adjustWithKey f k)
  insertWithKey f k v = withGridMap (GM.insertWithKey f k v)
  alter f k = withGridMap (GM.alter f k)
  filterWithKey f = withGridMap (GM.filterWithKey f)

withGridMap :: (gm p -> gm p) -> DSOM gm x k p -> DSOM gm x k p
withGridMap f s = s { gridMap=gm' }
    where gm = gridMap s
          gm' = f gm

-- | Extracts the grid and current models from the DSOM.
toGridMap :: GM.GridMap gm p => DSOM gm x k p -> gm p
toGridMap = gridMap

adjustNode
  :: (G.FiniteGrid (gm p), GM.GridMap gm p,
      k ~ G.Index (gm p), k ~ G.Index (GM.BaseGrid gm p),
      Ord k, Num x, Fractional x) => 
     gm p -> (p -> x -> p -> p) -> (p -> p -> x) -> (x -> x -> x) -> p -> k -> k
       -> (p -> p)
adjustNode gm fms fd fr target bmu k = fms target amount
  where diff = fd (gm GM.! k) target
        dist = scaleDistance (G.distance gm bmu k)
                 (G.maxPossibleDistance gm)
        amount = fr diff dist

scaleDistance :: (Num a, Fractional a) => Int -> Int -> a
scaleDistance d dMax
  | dMax == 0  = 0
  | otherwise = fromIntegral d / fromIntegral dMax

-- | Trains the specified node and the neighbourood around it to better
--   match a target.
--   Most users should use @train@, which automatically determines
--   the BMU and trains it and its neighbourhood.
trainNeighbourhood
  :: (G.FiniteGrid (gm p), GM.GridMap gm p,
      k ~ G.Index (gm p), k ~ G.Index (GM.BaseGrid gm p),
      Ord k, Num x, Fractional x) => 
      DSOM gm x t p -> k -> p -> DSOM gm x k p
trainNeighbourhood s bmu target = s { gridMap=gm' }
  where gm = gridMap s
        gm' = GM.mapWithKey (adjustNode gm fms fd fr target bmu) gm
        fms = makeSimilar s
        fd = difference s
        fr = (learningRate s) bmuDiff
        bmuDiff = (difference s) (gm GM.! bmu) target

justTrain
  :: (G.FiniteGrid (gm p), GM.GridMap gm p, GM.GridMap gm x,
      k ~ G.Index (gm p), k ~ G.Index (gm x),
      k ~ G.Index (GM.BaseGrid gm p), k ~ G.Index (GM.BaseGrid gm x),
      Ord k, Ord x, Num x, Fractional x) => 
     DSOM gm x t p -> p -> DSOM gm x k p
justTrain s p = trainNeighbourhood s bmu p
  where ds = GM.toList . GM.map (difference s p) $ gridMap s
        bmu = f ds
        f [] = error "DSOM has no models"
        f xs = fst $ minimumBy (comparing snd) xs

instance
  (GM.GridMap gm p, k ~ G.Index (GM.BaseGrid gm p), 
    G.FiniteGrid (gm p), GM.GridMap gm x, k ~ G.Index (gm p),
    k ~ G.Index (gm x), k ~ G.Index (GM.BaseGrid gm x), Ord k, Ord x,
    Num x, Fractional x) =>
   Classifier (DSOM gm) x k p where
  toList = GM.toList . gridMap
  numModels = G.tileCount . gridMap
  models = GM.elems . gridMap
  differences s p = GM.toList . GM.map (difference s p) $ gridMap s
  trainBatch s = foldl' justTrain s
  reportAndTrain s p = (bmu, ds, s')
    where ds = differences s p
          bmu = f ds
          f [] = error "DSOM has no models"
          f xs = fst $ minimumBy (comparing snd) xs
          s' = trainNeighbourhood s bmu p

-- | Configures a learning function that depends not on the time, but
--   on how good a model we already have for the target. If the
--   BMU is an exact match for the target, no learning occurs.
--   Usage is @'rougierLearningFunction' r p@, where @r@ is the
--   maximal learning rate (0 <= r <= 1), and @p@ is the elasticity.
--
--   NOTE: When using this learning function, ensure that
--   @abs . difference@ is always between 0 and 1, inclusive. Otherwise
--   you may get invalid learning rates.
rougierLearningFunction
  :: (Eq a, Ord a, Floating a) => a -> a -> (a -> a -> a -> a)
rougierLearningFunction r p bmuDiff diff dist
  | bmuDiff == 0         = 0
  | otherwise           = r * abs diff * exp (-k*k)
  where k = dist/(p*abs bmuDiff) 

