------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2014
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
import Data.Datamining.Pattern (Pattern(..))
import Data.Datamining.Clustering.Classifier(Classifier(..))
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | A function used to adjust the models in a classifier.
class LearningFunction f where
  type LearningRate f
  -- | @'rate' f t d@ returns the learning rate for a node.
  --   The parameter @f@ is the learning function.
  --   The parameter @t@ indicates how many patterns (or pattern
  --   batches) have previously been presented to the classifier.
  --   Typically this is used to make the learning rate decay over time.
  --   The parameter @d@ is the grid distance from the node being
  --   updated to the BMU (Best Matching Unit).
  --   The output is the learning rate for that node (the amount by
  --   which the node's model should be updated to match the target).
  --   The learning rate should be between zero and one.
  rate :: f -> LearningRate f -> LearningRate f -> LearningRate f

-- | A typical learning function for classifiers.
--   @'DecayingGaussian' r0 rf w0 wf tf@ returns a bell curve-shaped
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
data DecayingGaussian a = DecayingGaussian a a a a a
  deriving (Eq, Show, Generic)

instance (Floating a, Fractional a, Num a)
    => LearningFunction (DecayingGaussian a) where
  type LearningRate (DecayingGaussian a) = a
  rate (DecayingGaussian r0 rf w0 wf tf) t d = r * exp (-(d*d)/(2*w*w))
    where a = t/tf
          r = r0 * ((rf/r0)**a)
          w = w0 * ((wf/w0)**a)

-- | A learning function that only updates the BMU and has a constant
--   learning rate.
data StepFunction a = StepFunction a deriving (Eq, Show, Generic)

instance (Fractional a, Eq a)
  => LearningFunction (StepFunction a) where
  type LearningRate (StepFunction a) = a
  rate (StepFunction r) _ d = if d == 0 then r else 0.0

-- | A learning function that updates all nodes with the same, constant
--   learning rate. This can be useful for testing.
data ConstantFunction a = ConstantFunction a deriving (Eq, Show, Generic)

instance (Fractional a) => LearningFunction (ConstantFunction a) where
  type LearningRate (ConstantFunction a) = a
  rate (ConstantFunction r) _ _ = r

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
data SOM f t gm k p = SOM
  {
    -- | Maps patterns to tiles in a regular grid.
    --   In the context of a SOM, the tiles are called "nodes"
    gridMap :: gm p,
    -- | The function used to update the nodes.
    learningFunction :: f,
    -- | A counter used as a "time" parameter.
    --   If you create the SOM with a counter value @0@, and don't
    --   directly modify it, then the counter will represent the number
    --   of patterns that this SOM has classified.
    counter :: t
  } deriving (Eq, Show, Generic)

instance (F.Foldable gm) => F.Foldable (SOM f t gm k) where
  foldr f x g = F.foldr f x (gridMap g)

instance (G.Grid (gm p)) => G.Grid (SOM f t gm k p) where
  type Index (SOM f t gm k p) = G.Index (gm p)
  type Direction (SOM f t gm k p) = G.Direction (gm p)
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
    => GM.GridMap (SOM f t gm k) p where
  type BaseGrid (SOM f t gm k) p = GM.BaseGrid gm p
  toGrid = GM.toGrid . gridMap
  toMap = GM.toMap . gridMap
  mapWithKey = error "Not implemented"
  adjustWithKey f k s = s { gridMap=gm' }
    where gm = gridMap s
          gm' = GM.adjustWithKey f k gm

currentLearningFunction
  :: (LearningFunction f, Metric p ~ LearningRate f,
    Num (LearningRate f), Integral t)
      => SOM f t gm k p -> (LearningRate f -> Metric p)
currentLearningFunction s
  = rate (learningFunction s) (fromIntegral $ counter s)

-- | Extracts the grid and current models from the SOM.
--   A synonym for @'gridMap'@.
toGridMap :: GM.GridMap gm p => SOM f t gm k p -> gm p
toGridMap = gridMap

adjustNode
  :: (Pattern p, G.Grid g, k ~ G.Index g, Num t) =>
     g -> (t -> Metric p) -> p -> k -> k -> p -> p
adjustNode g f target bmu k = makeSimilar target (f d)
  where d = fromIntegral $ G.distance g bmu k

-- | Trains the specified node and the neighbourood around it to better
--   match a target.
--   Most users should use @train@, which automatically determines
--   the BMU and trains it and its neighbourhood.
trainNeighbourhood
  :: (Pattern p, G.Grid (gm p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p), LearningFunction f,
      Metric p ~ LearningRate f, Num (LearningRate f), Integral t) =>
     SOM f t gm k p -> G.Index (gm p) -> p -> SOM f t gm k p
trainNeighbourhood s bmu target = s { gridMap=gm' }
  where gm = gridMap s
        gm' = GM.mapWithKey (adjustNode gm f target bmu) gm
        f = currentLearningFunction s

incrementCounter :: Num t => SOM f t gm k p -> SOM f t gm k p
incrementCounter s = s { counter=counter s + 1}

justTrain
  :: (Ord (Metric p), Pattern p, G.Grid (gm p),
      GM.GridMap gm (Metric p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm (Metric p)) ~ G.Index (gm p),
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p), LearningFunction f,
      Metric p ~ LearningRate f, Num (LearningRate f), Integral t) =>
     SOM f t gm k p -> p -> SOM f t gm k p
justTrain s p = trainNeighbourhood s bmu p
  where ds = GM.toList . GM.map (p `difference`) $ gridMap s
        bmu = f ds
        f [] = error "SOM has no models"
        f xs = fst $ minimumBy (comparing snd) xs

instance
  (GM.GridMap gm p, k ~ G.Index (GM.BaseGrid gm p), Pattern p,
  G.Grid (gm p), GM.GridMap gm (Metric p), k ~ G.Index (gm p),
  k ~ G.Index (GM.BaseGrid gm (Metric p)), Ord (Metric p),
  LearningFunction f, Metric p ~ LearningRate f, Num (LearningRate f),
  Integral t)
    => Classifier (SOM f t gm) k p where
  toList = GM.toList . gridMap
  numModels = G.tileCount . gridMap
  models = GM.elems . gridMap
  differences s p = GM.toList . GM.map (p `difference`) $ gridMap s
  trainBatch s = incrementCounter . foldl' justTrain s
  reportAndTrain s p = (bmu, ds, incrementCounter s')
    where ds = differences s p
          bmu = fst $ minimumBy (comparing snd) ds
          s' = trainNeighbourhood s bmu p
