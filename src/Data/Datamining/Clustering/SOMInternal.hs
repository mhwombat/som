------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2013
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
data SOM gm k p = SOM
  {
    sGridMap :: gm p,
    sLearningFunction :: Int -> Int -> Metric p,
    sCounter :: Int
  } deriving Generic

instance (F.Foldable gm) => F.Foldable (SOM gm k) where
  foldr f x g = F.foldr f x (sGridMap g)

instance (G.Grid (gm p)) => G.Grid (SOM gm k p) where
  type Index (SOM gm k p) = G.Index (gm p)
  type Direction (SOM gm k p) = G.Direction (gm p)
  indices = G.indices . sGridMap
  distance = G.distance . sGridMap
  neighbours = G.neighbours . sGridMap
  contains = G.contains . sGridMap
  viewpoint = G.viewpoint . sGridMap
  directionTo = G.directionTo . sGridMap
  tileCount = G.tileCount . sGridMap
  null = G.null . sGridMap
  nonNull = G.nonNull . sGridMap

instance (F.Foldable gm, GM.GridMap gm p, G.Grid (GM.BaseGrid gm p)) => GM.GridMap (SOM gm k) p where
  type BaseGrid (SOM gm k) p = GM.BaseGrid gm p
  toGrid = GM.toGrid . sGridMap
  toMap = GM.toMap . sGridMap
  mapWithKey = error "Not implemented"
  adjustWithKey f k s = s { sGridMap=gm' }
    where gm = sGridMap s
          gm' = GM.adjustWithKey f k gm

currentLearningFunction :: SOM gm k p -> (Int -> Metric p)
currentLearningFunction s = (sLearningFunction s) (sCounter s)

-- | Extracts the grid and current models from the SOM.
toGridMap :: GM.GridMap gm p => SOM gm k p -> gm p
toGridMap = sGridMap

adjustNode
  :: (Pattern p, G.Grid g, k ~ G.Index g) =>
     g -> (Int -> Metric p) -> p -> k -> k -> p -> p
adjustNode g f target bmu k = makeSimilar target (f d)
  where d = G.distance g bmu k

-- | Trains the specified node and the neighbourood around it to better
--   match a target.
--   Most users should use @train@, which automatically determines
--   the BMU and trains it and its neighbourhood.
trainNeighbourhood
  :: (Pattern p, G.Grid (gm p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p)) =>
     SOM gm k p -> G.Index (gm p) -> p -> SOM gm k p
trainNeighbourhood s bmu target = s { sGridMap=gm' }
  where gm = sGridMap s
        gm' = GM.mapWithKey (adjustNode gm f target bmu) gm
        f = currentLearningFunction s

incrementCounter :: SOM gm k p -> SOM gm k p
incrementCounter s = setCounter (sCounter s + 1) s

counter :: SOM gm k p -> Int
counter = sCounter

setCounter :: Int -> SOM gm k p -> SOM gm k p
setCounter k s = s { sCounter = k }

justTrain
  :: (Ord (Metric p), Pattern p, G.Grid (gm p),
      GM.GridMap gm (Metric p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm (Metric p)) ~ G.Index (gm p),
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p)) =>
     SOM gm k p -> p -> SOM gm k p
justTrain s p = trainNeighbourhood s bmu p
  where ds = GM.toList . GM.map (p `difference`) . sGridMap $ s
        bmu = fst . minimumBy (comparing snd) $ ds

instance
  (GM.GridMap gm p, k ~ G.Index (GM.BaseGrid gm p), Pattern p,
  G.Grid (gm p), GM.GridMap gm (Metric p), k ~ G.Index (gm p),
  k ~ G.Index (GM.BaseGrid gm (Metric p)), Ord (Metric p)) =>
    Classifier (SOM gm) k p where
  toList = GM.toList . sGridMap
  numModels = G.tileCount . sGridMap
  models = GM.elems . sGridMap
  differences s p = GM.toList . GM.map (p `difference`) . sGridMap $ s
  trainBatch s = incrementCounter . foldl' justTrain s
  reportAndTrain s p = (bmu, ds, incrementCounter s')
    where ds = differences s p
          bmu = fst . minimumBy (comparing snd) $ ds
          s' = trainNeighbourhood s bmu p


-- | Creates a classifier with a default (bell-shaped) learning
--   function. Usage is @'defaultSOM' gm r0 rf w0 wf tf@, where:
--
--   [@gm@] The geometry and initial models for this classifier.
--   A reasonable choice here is @'lazyGridMap' g ps@, where @g@ is a
--   @'HexHexGrid'@, and @ps@ is a set of random patterns.
--
--   [@r0@] See description in @'decayingGaussian2'@.
--
--   [@rf@] See description in @'decayingGaussian2'@.
--
--   [@w0@] See description in @'decayingGaussian2'@.
--
--   [@wf@] See description in @'decayingGaussian2'@.
--
--   [@tf@] See description in @'decayingGaussian2'@.
defaultSOM
  :: Floating (Metric p) => gm p -> Metric p -> Metric p -> Metric p ->
     Metric p -> Int -> SOM gm k p
defaultSOM gm r0 rf w0 wf tf =
  SOM {
        sGridMap=gm,
        sLearningFunction=decayingGaussian2 r0 rf w0 wf tf,
        sCounter=0
      }

-- | Creates a classifier with a custom learning function.
--   Usage is @'customSOM' gm g@, where:
--
--   [@gm@] The geometry and initial models for this classifier.
--   A reasonable choice here is @'lazyGridMap' g ps@, where @g@ is a
--   @'HexHexGrid'@, and @ps@ is a set of random patterns.
--
--   [@f@] A function used to adjust the models in the classifier.
--   This function will be invoked with two parameters.
--   The first parameter will indicate how many patterns (or pattern
--   batches) have previously been presented to this classifier.
--   Typically this is used to make the learning rate decay over time.
--   The second parameter to the function is the grid distance from
--   the node being updated to the BMU (Best Matching Unit).
--   The output is the learning rate for that node (the amount by
--   which the node's model should be updated to match the target).
--   The learning rate should be between zero and one.
customSOM :: gm p -> (Int -> Int -> Metric p) -> SOM gm k p
customSOM gm f =
  SOM {
        sGridMap=gm,
        sLearningFunction=f,
        sCounter=0
      }

-- | Configures one possible learning function for classifiers.
--   @'decayingGaussian' r0 w0 tMax@ returns a bell curve-shaped
--   function. At time zero, the maximum learning rate (applied to the
--   BMU) is @r0@, and the neighbourhood width is @w0@. Over time the
--   neighbourhood width shrinks and the learning rate tapers off.
decayingGaussian
  :: Floating a => a -> a -> Int -> (Int -> Int -> a)
decayingGaussian r w0 tMax t d = r * s * exp (-(d'*d')/(2*w0*w0*s*s))
    where s = exp (-t'/tMax')
          t' = fromIntegral t
          tMax' = fromIntegral tMax
          d' = fromIntegral d

-- | Configures a typical learning function for classifiers.
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
decayingGaussian2 :: Floating a => a -> a -> a -> a -> Int -> (Int -> Int -> a)
decayingGaussian2 r0 rf w0 wf tf t d = r * exp (-(d'*d')/(w*w))
    where a = t'/tf'
          r = r0 * ((rf/r0)**a)
          w = w0 * ((wf/w0)**a)
          t' = fromIntegral t
          tf' = fromIntegral tf
          d' = fromIntegral d
