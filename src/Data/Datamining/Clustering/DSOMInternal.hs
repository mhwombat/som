------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.DSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2014
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
import Data.Datamining.Pattern (Pattern(..))
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
data DSOM gm k p = DSOM
  {
    sGridMap :: gm p,
    sLearningFunction :: (Metric p -> Metric p -> Metric p -> Metric p)
  }

instance (F.Foldable gm) => F.Foldable (DSOM gm k) where
  foldr f x g = F.foldr f x (sGridMap g)

instance (G.Grid (gm p)) => G.Grid (DSOM gm k p) where
  type Index (DSOM gm k p) = G.Index (gm p)
  type Direction (DSOM gm k p) = G.Direction (gm p)
  indices = G.indices . sGridMap
  distance = G.distance . sGridMap
  neighbours = G.neighbours . sGridMap
  contains = G.contains . sGridMap
  viewpoint = G.viewpoint . sGridMap
  directionTo = G.directionTo . sGridMap
  tileCount = G.tileCount . sGridMap
  null = G.null . sGridMap
  nonNull = G.nonNull . sGridMap

instance
  (F.Foldable gm, GM.GridMap gm p, G.FiniteGrid (GM.BaseGrid gm p)) =>
    GM.GridMap (DSOM gm k) p where
  type BaseGrid (DSOM gm k) p = GM.BaseGrid gm p
  toGrid = GM.toGrid . sGridMap
  toMap = GM.toMap . sGridMap
  mapWithKey = error "Not implemented"
  delete k = withGridMap (GM.delete k)
  adjustWithKey f k = withGridMap (GM.adjustWithKey f k)
  insertWithKey f k v = withGridMap (GM.insertWithKey f k v)
  alter f k = withGridMap (GM.alter f k)
  filterWithKey f = withGridMap (GM.filterWithKey f)

withGridMap :: (gm p -> gm p) -> DSOM gm k p -> DSOM gm k p
withGridMap f s = s { sGridMap=gm' }
    where gm = sGridMap s
          gm' = f gm

-- | Extracts the grid and current models from the DSOM.
toGridMap :: GM.GridMap gm p => DSOM gm k p -> gm p
toGridMap = sGridMap

adjustNode
  :: (Pattern p, G.FiniteGrid (gm p), GM.GridMap gm p,
      k ~ G.Index (gm p), Ord k, k ~ G.Index (GM.BaseGrid gm p),
      Num (Metric p), Fractional (Metric p)) => 
     gm p -> (Metric p -> Metric p -> Metric p) -> p -> k -> k -> p -> p
adjustNode gm f target bmu k = makeSimilar target amount
  where diff = difference (gm GM.! k) target
        dist = scaleDistance (G.distance gm bmu k)
                 (G.maxPossibleDistance gm)
        amount = f diff dist

scaleDistance :: (Num a, Fractional a) => Int -> Int -> a
scaleDistance d dMax
  | dMax == 0  = 0
  | otherwise = fromIntegral d / fromIntegral dMax

-- | Trains the specified node and the neighbourood around it to better
--   match a target.
--   Most users should use @train@, which automatically determines
--   the BMU and trains it and its neighbourhood.
trainNeighbourhood
  :: (Pattern p, G.FiniteGrid (gm p), GM.GridMap gm p, Num (Metric p),
      Ord k, k ~ G.Index (gm p),
      k ~ G.Index (GM.BaseGrid gm p), Fractional (Metric p)) =>
     DSOM gm t p -> k -> p -> DSOM gm k p
trainNeighbourhood s bmu target = s { sGridMap=gm' }
  where gm = sGridMap s
        gm' = GM.mapWithKey (adjustNode gm f target bmu) gm
        f = (sLearningFunction s) bmuDiff
        bmuDiff = difference (gm GM.! bmu) target

justTrain
  :: (Pattern p, G.FiniteGrid (gm p), GM.GridMap gm p,
      Num (Metric p), Ord (Metric p), Ord (G.Index (gm p)),
      GM.GridMap gm (Metric p), Fractional (Metric p),
      G.Index (GM.BaseGrid gm (Metric p)) ~ G.Index (gm p),
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p)) =>
     DSOM gm t p -> p -> DSOM gm (G.Index (gm p)) p
justTrain s p = trainNeighbourhood s bmu p
  where ds = GM.toList . GM.map (p `difference`) $ sGridMap s
        bmu = f ds
        f [] = error "DSOM has no models"
        f xs = fst $ minimumBy (comparing snd) xs

instance
  (GM.GridMap gm p, k ~ G.Index (GM.BaseGrid gm p), Pattern p,
    G.FiniteGrid (gm p), GM.GridMap gm (Metric p), k ~ G.Index (gm p),
    k ~ G.Index (GM.BaseGrid gm (Metric p)), Ord k, Ord (Metric p),
    Num (Metric p), Fractional (Metric p)) =>
   Classifier (DSOM gm) k p where
  toList = GM.toList . sGridMap
  numModels = G.tileCount . sGridMap
  models = GM.elems . sGridMap
  differences s p = GM.toList . GM.map (p `difference`) $ sGridMap s
  trainBatch s = foldl' justTrain s
  reportAndTrain s p = (bmu, ds, s')
    where ds = differences s p
          bmu = f ds
          f [] = error "DSOM has no models"
          f xs = fst $ minimumBy (comparing snd) xs
          s' = trainNeighbourhood s bmu p


-- | Creates a classifier with a default (bell-shaped) learning
--   function. Usage is @'defaultDSOM' gm r w t@, where:
--
--   [@gm@] The geometry and initial models for this classifier.
--   A reasonable choice here is @'lazyGridMap' g ps@, where @g@ is a
--   @'HexHexGrid'@, and @ps@ is a set of random patterns.
--
--   [@r@] and [@p@] are the first two parameters to the
--   @'rougierLearningFunction'@.
defaultDSOM
  :: (Eq (Metric p), Ord (Metric p), Floating (Metric p)) =>
     gm p -> Metric p -> Metric p -> DSOM gm k p
defaultDSOM gm r p =
  DSOM {
        sGridMap=gm,
        sLearningFunction=rougierLearningFunction r p
      }

-- | Creates a classifier with a custom learning function.
--   Usage is @'customDSOM' gm g@, where:
--
--   [@gm@] The geometry and initial models for this classifier.
--   A reasonable choice here is @'lazyGridMap' g ps@, where @g@ is a
--   @'HexHexGrid'@, and @ps@ is a set of random patterns.
--
--   [@f@] A function used to determine the learning rate (for
--   adjusting the models in the classifier).
--   This function will be invoked with three parameters.
--   The first parameter will indicate how different the BMU is from
--   the input pattern.
--   The second parameter indicates how different the pattern of the
--   node currently being trained is from the input pattern.
--   The third parameter is the grid distance from the BMU to the node
--   currently being trained, as a fraction of the maximum grid
--   distance.
--   The output is the learning rate for that node (the amount by
--   which the node's model should be updated to match the target).
--   The learning rate should be between zero and one.
customDSOM
  :: gm p -> (Metric p -> Metric p -> Metric p -> Metric p) -> DSOM gm k p
customDSOM gm f =
  DSOM {
        sGridMap=gm,
        sLearningFunction=f
      }

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

