------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SSOM@ internals. Most developers should
-- use @SSOM@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, DeriveGeneric #-}

module Data.Datamining.Clustering.SSOMInternal where

import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Data.Datamining.Clustering.Classifier(Classifier(..))
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | A typical learning function for classifiers.
--   @'exponential' r0 d t@ returns the learning rate at time @t@.
--   When @t = 0@, the learning rate is @r0@.
--   Over time the learning rate decays exponentially; the decay rate is
--   @d@.
--   Normally the parameters are chosen such that:
--
--   * 0 < r0 < 1
--
--   * 0 < d
--
--   where << means "is much smaller than" (not the Haskell @<<@
--   operator!)
exponential :: Floating a => a -> a -> a -> a
exponential r0 d t = r0 * exp (-d*t)

-- | A Simplified Self-Organising Map (SSOM).
--   @x@ is the type of the learning rate and the difference metric.
--   @t@ is the type of the counter.
--   @k@ is the type of the model indices.
--   @p@ is the type of the input patterns and models.
data SSOM t x k p = SSOM
  {
    -- | Maps patterns to nodes.
    sMap :: M.Map k p,
    -- | A function which determines the learning rate for a node.
    --   The input parameter indicates how many patterns (or pattern
    --   batches) have previously been presented to the classifier.
    --   Typically this is used to make the learning rate decay over
    --   time.
    --   The output is the learning rate for that node (the amount by
    --   which the node's model should be updated to match the target).
    --   The learning rate should be between zero and one.
    learningRate :: t -> x,
    -- | A function which compares two patterns and returns a 
    --   /non-negative/ number representing how different the patterns
    --   are.
    --   A result of @0@ indicates that the patterns are identical.
    difference :: p -> p -> x,
    -- | A function which updates models.
    --   For example, if this function is @f@, then
    --   @f target amount pattern@ returns a modified copy of @pattern@
    --   that is more similar to @target@ than @pattern@ is.
    --   The magnitude of the adjustment is controlled by the @amount@
    --   parameter, which should be a number between 0 and 1.
    --   Larger values for @amount@ permit greater adjustments.
    --   If @amount@=1, the result should be identical to the @target@.
    --   If @amount@=0, the result should be the unmodified @pattern@.
    makeSimilar :: p -> x -> p -> p,
    -- | A counter used as a "time" parameter.
    --   If you create the SSOM with a counter value @0@, and don't
    --   directly modify it, then the counter will represent the number
    --   of patterns that this SSOM has classified.
    counter :: t
  } deriving (Generic)

-- | Extracts the current models from the SSOM.
--   A synonym for @'sMap'@.
toMap :: SSOM t x k p -> M.Map k p
toMap = sMap

-- | Trains the specified node to better match a target.
--   Most users should use @'train'@, which automatically determines
--   the BMU and trains it.
trainNode
  :: (Num t, Ord k)
      => SSOM t x k p -> k -> p -> SSOM t x k p
trainNode s k target = s { sMap=gm' }
  where gm = sMap s
        gm' = M.adjust (makeSimilar s target r) k gm
        r = (learningRate s) (counter s)

incrementCounter :: Num t => SSOM t x k p -> SSOM t x k p
incrementCounter s = s { counter=counter s + 1}

justTrain
  :: (Num t, Ord k, Ord x)
      => SSOM t x k p -> p -> SSOM t x k p
justTrain s p = trainNode s bmu p
  where ds = M.toList . M.map (difference s p) . toMap $ s
        bmu = f ds
        f [] = error "SSOM has no models"
        f xs = fst $ minimumBy (comparing snd) xs

instance
  (Num t, Ord x, Num x, Ord k)
    => Classifier (SSOM t) x k p where
  toList = M.toList . toMap
  -- TODO: If the # of models is fixed, make more efficient
  numModels = length . M.keys . sMap
  models = M.elems . toMap
  differences s p = M.toList . M.map (difference s p) $ toMap s
  trainBatch s = incrementCounter . foldl' justTrain s
  reportAndTrain s p = (bmu, ds, s')
    where ds = differences s p
          bmu = fst $ minimumBy (comparing snd) ds
          s' = incrementCounter . trainNode s bmu $ p
