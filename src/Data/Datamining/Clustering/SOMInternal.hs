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
{-# LANGUAGE UnicodeSyntax, TypeFamilies, FlexibleContexts, 
    FlexibleInstances, MultiParamTypeClasses #-}

module Data.Datamining.Clustering.SOMInternal
  (
    SOM(..),
    defaultSOM,
    customSOM,
    gaussian,
    decayingGaussian,
    toGridMap
  ) where

import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Math.Geometry.Grid as G (Grid(..))
import qualified Math.Geometry.GridMap as GM (GridMap(..))
import Data.Datamining.Pattern (Pattern(..))
import Data.Datamining.Clustering.Classifier(Classifier(..))
import Prelude hiding (lookup)

data SOM gm k p = SOM
  {
    sGridMap ∷ gm p,
    sLearningFunction ∷ Int → Int → Metric p,
    sCounter ∷ Int
  }

currentLearningFunction ∷ SOM gm k p → (Int → Metric p)
currentLearningFunction s = (sLearningFunction s) (sCounter s)

-- | Extract the grid and current models from the SOM.
toGridMap ∷ GM.GridMap gm p ⇒ SOM gm k p → gm p
toGridMap = sGridMap

adjustNode
  ∷ (Pattern p, G.Grid g, k ~ G.Index g) ⇒
     g → (Int → Metric p) → p → k → k → p → p
adjustNode g f target bmu k = makeSimilar target (f d)
  where d = G.distance g bmu k

trainWithBMU
  ∷ (Pattern p, G.Grid (gm p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p)) ⇒
     SOM gm k p → G.Index (gm p) → p → SOM gm k p
trainWithBMU s bmu target = s { sGridMap=gm' }
  where gm = sGridMap s
        gm' = GM.mapWithKey (adjustNode gm f target bmu) gm
        f = currentLearningFunction s

justTrain
  ∷ (Ord (Metric p), Pattern p, G.Grid (gm p),
      GM.GridMap gm (Metric p), GM.GridMap gm p,
      G.Index (GM.BaseGrid gm (Metric p)) ~ G.Index (gm p),
      G.Index (GM.BaseGrid gm p) ~ G.Index (gm p)) ⇒
     SOM gm k p → p → SOM gm k p
justTrain s p = trainWithBMU s bmu p
  where ds = GM.toList . GM.map (p `difference`) . sGridMap $ s
        bmu = fst . minimumBy (comparing snd) $ ds

instance 
  (GM.GridMap gm p, k ~ G.Index (GM.BaseGrid gm p), Pattern p, 
  G.Grid (gm p), GM.GridMap gm (Metric p), k ~ G.Index (gm p),
  k ~ G.Index (GM.BaseGrid gm (Metric p)), Ord (Metric p)) ⇒ 
    Classifier (SOM gm) k p where
  toList = GM.toList . sGridMap
  numModels = G.tileCount . sGridMap
  models = GM.elems . sGridMap
  differences s p = GM.toList . GM.map (p `difference`) . sGridMap $ s
  trainBatch s ps = (foldl' justTrain s ps) {sCounter=sCounter s + 1}
  reportAndTrain s p = (bmu, ds, s'')
    where ds = differences s p
          bmu = fst . minimumBy (comparing snd) $ ds
          s' = trainWithBMU s bmu p
          s'' = s' { sCounter=sCounter s + 1}


-- Creates a classifier with a default (bell-shaped) learning function.
defaultSOM 
  ∷ Floating (Metric p) ⇒
  -- | The geometry and initial models for this classifier.
  --   A reasonable choice here is 'lazyGridMap g ps', where 'g' is a
  --   @'Math.Geometry.Grid.HexHexGrid'@, and 'ps' is a set of
  --   random patterns.
  gm p →
  -- | The learning rate to be applied to the BMU (Best Matching Unit)
  --   at "time" zero. The BMU is the model which best matches the
  --   current target pattern.
  Metric p →
  -- | The width of the bell curve at "time" zero.
  Metric p →
  -- | After this time, any learning done by the classifier will be
  --   negligible. Recommend setting this parameter to the number of
  --   patterns (or pattern batches) that will be presented to the
  --   classifier. An estimate is fine.
  Int →
  -- | The result
  SOM gm k p
defaultSOM gm r w t = 
  SOM { 
        sGridMap=gm, 
        sLearningFunction=decayingGaussian r w t, 
        sCounter=0
      }

-- Creates a classifier with a custom learning function.
customSOM ∷ 
  -- | The geometry and initial models for this classifier.
  --   A reasonable choice here is 'lazyGridMap g ps', where 'g' is a
  --   @'Math.Geometry.Grid.HexHexGrid'@, and 'ps' is a set of
  --   random patterns.
  gm p →
  -- | A function used to adjust the models in the classifier.
  --   This function will be invoked with two parameters.
  --   The first parameter will indicate how many patterns (or pattern 
  --   batches) have previously been presented to this classifier. 
  --   Typically this is used to make the learning rate decay over time.
  --   The second parameter to the function is the grid distance from
  --   the node being updated to the BMU (Best Matching Unit).
  --   The output is the learning rate for that node (the amount by
  --   which the node's model should be updated to match the target).
  --   The learning rate should be between zero and one.
  (Int → Int → Metric p) →
  -- | The result
  SOM gm k p
customSOM gm f = 
  SOM {
        sGridMap=gm,
        sLearningFunction=f,
        sCounter=0
      }


-- | Calculates @r/e/^(-d^2/2w^2)@.
--   This form of the Gaussian function is useful as a learning rate
--   function. In @'gaussian' r w d@, @r@ specifies the highest learning
--   rate, which will be applied to the SOM node that best matches the
--   input pattern. The learning rate applied to other nodes will be 
--   applied based on their distance @d@ from the best matching node. 
--   The value @w@ controls the \'width\' of the Gaussian. Higher values
--   of @w@ cause the learning rate to fall off more slowly with 
--   distance @d@.
gaussian ∷ Floating a ⇒ a → a → Int → a
gaussian r w d = r * exp (-d'*d'/(2*w*w))
  where d' = fromIntegral d

-- | Configures a typical learning function for classifiers.
--   @'decayingGaussian r w0 tMax' returns a bell curve-shaped function.
--   At time zero, the maximum learning rate (applied to the BMU) is 
--   @r@, and the neighbourhood width is @w@. Over time the bell curve
--   shrinks and the learning rate tapers off, until at time @tMax@,
--   the learning rate is negligible.
decayingGaussian
  ∷ Floating a ⇒ a → a → Int → (Int → Int → a)
decayingGaussian r w0 tMax = 
  \t d → let t' = fromIntegral t in gaussian r w0 d * exp (-t'/tMax')
  where tMax' = fromIntegral tMax


