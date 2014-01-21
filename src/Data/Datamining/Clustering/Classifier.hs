------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.Classifier
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for identifying patterns in data.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Data.Datamining.Clustering.Classifier
  (
    Classifier(..)
  ) where

import Data.Datamining.Pattern (Pattern, Metric)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- | A machine which learns to classify input patterns. 
--   Minimal complete definition: @trainBatch@, @reportAndTrain@.
class Classifier (c :: * -> * -> *) k p where
  -- | Returns a list of index\/model pairs.
  toList :: c k p -> [(k, p)]

  -- | Returns the number of models this classifier can learn.
  numModels :: c k p -> Int

  -- | Returns the current models of the classifier.
  models :: c k p -> [p]

  -- | @'differences' c target@ returns the indices of all nodes in 
  --   @c@, paired with the difference between @target@ and the 
  --   node's model.
  differences :: (Pattern p, v ~ Metric p) => c k p -> p -> [(k, v)]

  -- | @classify c target@ returns the index of the node in @c@ 
  --   whose model best matches the @target@.
  classify :: (Pattern p, Ord v, v ~ Metric p) => c k p -> p -> k
  classify c p = f $ differences c p
    where f [] = error "classifier has no models"
          f xs = fst $ minimumBy (comparing snd) xs

  -- | @'train' c target@ returns a modified copy
  --   of the classifier @c@ that has partially learned the @target@.
  train
    :: (Ord v, v ~ Metric p) => 
      c k p -> p -> c k p
  train c p = c'
    where (_, _, c') = reportAndTrain c p

  -- | @'trainBatch' c targets@ returns a modified copy
  --   of the classifier @c@ that has partially learned the @targets@.
  trainBatch :: c k p -> [p] -> c k p

  -- | @'classifyAndTrain' c target@ returns a tuple containing the
  --   index of the node in @c@ whose model best matches the input
  --   @target@, and a modified copy of the classifier @c@ that has
  --   partially learned the @target@. Invoking @classifyAndTrain c p@
  --   may be faster than invoking @(p `classify` c, train c p)@, but 
  --   they
  --   should give identical results.
  classifyAndTrain 
    :: (Ord v, v ~ Metric p) => 
      c k p -> p -> (k, c k p)
  classifyAndTrain c p = (bmu, c')
    where (bmu, _, c') = reportAndTrain c p

  -- | @'diffAndTrain' c target@ returns a tuple containing:
  --   1. The indices of all nodes in @c@, paired with the difference
  --      between @target@ and the node's model
  --   2. A modified copy of the classifier @c@ that has partially
  --      learned the @target@.
  --   Invoking @diffAndTrain c p@ may be faster than invoking
  --   @(p `diff` c, train c p)@, but they should give identical
  --   results.
  diffAndTrain
    :: (Ord v, v ~ Metric p) => 
      c k p -> p -> ([(k, v)], c k p)
  diffAndTrain c p = (ds, c')
    where (_, ds, c') = reportAndTrain c p

  -- | @'reportAndTrain' c f target@ returns a tuple containing:
  --   1. The index of the node in @c@ whose model best matches the
  --      input @target@
  --   2. The indices of all nodes in @c@, paired with the difference
  --      between @target@ and the node's model
  --   3. A modified copy of the classifier @c@ that has partially
  --      learned the @target@
  --   Invoking @diffAndTrain c p@ may be faster than invoking
  --   @(p `diff` c, train c p)@, but they should give identical
  --   results.
  reportAndTrain 
    :: (Ord v, v ~ Metric p) => 
      c k p -> p -> (k, [(k, v)], c k p)


