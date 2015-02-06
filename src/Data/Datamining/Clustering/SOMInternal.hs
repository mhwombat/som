------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SOM@ internals. Most developers should
-- use @SOM@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, DeriveGeneric #-}

module Data.Datamining.Clustering.SOMInternal where

import qualified Data.Foldable as F (Foldable, foldr)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Math.Geometry.Grid as G (Grid(..))
import qualified Math.Geometry.GridMap as GM (GridMap(..))
import Data.Datamining.Clustering.Classifier(Classifier(..))
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | A typical learning function for classifiers.
--   @'decayingGaussian' r0 rf w0 wf tf@ returns a bell curve-shaped
--   function. At time zero, the maximum learning rate (applied to the
--   BMU) is @r0@, and the neighbourhood width is @w0@. Over time the
--   bell curve shrinks and the learning rate tapers off, until at time
--   @tf@, the maximum learning rate (applied to the BMU) is @rf@,
--   and the neighbourhood width is @wf@. Normally the parameters
--   should be chosen such that:
--
--   * 0 < rf << r0 < 1
--
--   * 0 < wf << w0
--
--   * 0 < tf
--
--   where << means "is much smaller than" (not the Haskell @<<@
--   operator!)
decayingGaussian :: Floating x => x -> x -> x -> x -> x -> x -> x -> x
decayingGaussian r0 rf w0 wf tf t d = r * exp (-x/y)
  where a = t / tf
        r = r0 * ((rf/r0)**a)
        w = w0 * ((wf/w0)**a)
        x =  (d*d)
        y =  (2*w*w)

-- | A learning function that only updates the BMU and has a constant
--   learning rate.
stepFunction :: (Num d, Fractional x, Eq d) => x -> t -> d -> x
stepFunction r _ d = if d == 0 then r else 0.0

-- | A learning function that updates all nodes with the same, constant
--   learning rate. This can be useful for testing.
constantFunction :: x -> t -> d -> x
constantFunction r _ _ = r

-- | A Self-Organising Map (SOM).
--
--   Although @SOM@ implements @GridMap@, most users will only need the
--   interface provided by @Data.Datamining.Clustering.Classifier@. If
--   you chose to use the @GridMap@ functions, please note:
--
--   1. The functions @adjust@, and @adjustWithKey@ do not increment the
--      counter. You can do so manually with @incrementCounter@.
--
--   2. The functions @map@ and @mapWithKey@ are not implemented (they
--      just return an @error@). It would be problematic to implement
--      them because the input SOM and the output SOM would have to have
--      the same @Metric@ type.
data SOM t d gm x k p = SOM
  {
    -- | Maps patterns to tiles in a regular grid.
    --   In the context of a SOM, the tiles are called "nodes"
    gridMap :: gm p,
    -- | A function which determines the how quickly the SOM learns.
    --   For example, if the function is @f@, then @f t d@ returns the
    --   learning rate for a node.
    --   The parameter @t@ indicates how many patterns (or pattern
    --   batches) have previously been presented to the classifier.
    --   Typically this is used to make the learning rate decay over
    --   time.
    --   The parameter @d@ is the grid distance from the node being
    --   updated to the BMU (Best Matching Unit).
    --   The output is the learning rate for that node (the amount by
    --   which the node's model should be updated to match the target).
    --   The learning rate should be between zero and one.
    learningRate :: t -> d -> x,
    -- | A function which compares two patterns and returns a 
    --   /non-negative/ numberrepresenting how different the patterns
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
    makeSimilar :: p -> x -> p -> p,
    -- | A counter used as a "time" parameter.
    --   If you create the SOM with a counter value @0@, and don't
    --   directly modify it, then the counter will represent the number
    --   of patterns that this SOM has classified.
    counter :: t
  } deriving (Generic)

instance (F.Foldable gm) => F.Foldable (SOM t d gm x k) where
  foldr f x g = F.foldr f x (gridMap g)

instance (G.Grid (gm p)) => G.Grid (SOM t d gm x k p) where
  type Index (SOM t d gm x k p) = G.Index (gm p)
  type Direction (SOM t d gm x k p) = G.Direction (gm p)
  indices = G.indices . gridMap
  distance = G.distance . gridMap
  neighbours = G.neighbours . gridMap
  contains = G.contains . gridMap
  viewpoint = G.viewpoint . gridMap
  directionTo = G.directionTo . gridMap
  tileCount = G.tileCount . gridMap
  null = G.null . gridMap
  nonNull = G.nonNull . gridMap

instance (F.Foldable gm, GM.GridMap gm p, G.Grid (GM.BaseGrid gm p))
    => GM.GridMap (SOM t d gm x k) p where
  type BaseGrid (SOM t d gm x k) p = GM.BaseGrid gm p
  toGrid = GM.toGrid . gridMap
  toMap = GM.toMap . gridMap
  mapWithKey = error "Not implemented"
  delete k = withGridMap (GM.delete k)
  adjustWithKey f k = withGridMap (GM.adjustWithKey f k)
  insertWithKey f k v = withGridMap (GM.insertWithKey f k v)
  alter f k = withGridMap (GM.alter f k)
  filterWithKey f = withGridMap (GM.filterWithKey f)

withGridMap :: (gm p -> gm p) -> SOM t d gm x k p -> SOM t d gm x k p
withGridMap f s = s { gridMap=gm' }
    where gm = gridMap s
          gm' = f gm

currentLearningFunction
  :: (Num t)
    => SOM t d gm x k p -> (d -> x)
currentLearningFunction s
  = (learningRate s) (counter s)

-- | Extracts the grid and current models from the SOM.
--   A synonym for @'gridMap'@.
toGridMap :: GM.GridMap gm p => SOM t d gm x k p -> gm p
toGridMap = gridMap

adjustNode
  :: (G.Grid g, k ~ G.Index g, Num t) =>
     g -> (t -> x) -> (p -> x -> p -> p) -> p -> k -> k -> p -> p
adjustNode g rateF adjustF target bmu k = adjustF target (rateF d)
  where d = fromIntegral $ G.distance g bmu k

-- | Trains the specified node and the neighbourood around it to better
--   match a target.
--   Most users should use @'train'@, which automatically determines
--   the BMU and trains it and its neighbourhood.
trainNeighbourhood
  :: (G.Grid (gm p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p), Num t, Num x,
      Num d) =>
     SOM t d gm x k p -> G.Index (gm p) -> p -> SOM t d gm x k p
trainNeighbourhood s bmu target = s { gridMap=gm' }
  where gm = gridMap s
        gm' = GM.mapWithKey (adjustNode gm f1 f2 target bmu) gm
        f1 = currentLearningFunction s
        f2 = makeSimilar s

incrementCounter :: Num t => SOM t d gm x k p -> SOM t d gm x k p
incrementCounter s = s { counter=counter s + 1}

justTrain
  :: (Ord x, G.Grid (gm p), GM.GridMap gm x, GM.GridMap gm p,
      G.Index (GM.BaseGrid gm x) ~ G.Index (gm p),
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p), Num t, Num x,
      Num d) =>
     SOM t d gm x k p -> p -> SOM t d gm x k p
justTrain s p = trainNeighbourhood s bmu p
  where ds = GM.toList . GM.map (difference s p) $ gridMap s
        bmu = f ds
        f [] = error "SOM has no models"
        f xs = fst $ minimumBy (comparing snd) xs

instance
  (GM.GridMap gm p, k ~ G.Index (GM.BaseGrid gm p), G.Grid (gm p),
  GM.GridMap gm x, k ~ G.Index (gm p), k ~ G.Index (GM.BaseGrid gm x),
  Num t, Ord x, Num x, Num d)
    => Classifier (SOM t d gm) x k p where
  toList = GM.toList . gridMap
  numModels = G.tileCount . gridMap
  models = GM.elems . gridMap
  differences s p = GM.toList . GM.map (difference s p) $ gridMap s
  trainBatch s = incrementCounter . foldl' justTrain s
  reportAndTrain s p = (bmu, ds, incrementCounter s')
    where ds = differences s p
          bmu = fst $ minimumBy (comparing snd) ds
          s' = trainNeighbourhood s bmu p
