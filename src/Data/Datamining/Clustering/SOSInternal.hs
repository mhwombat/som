------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOSInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SOS@ internals. Most developers should
-- use @SOS@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, DeriveAnyClass, DeriveGeneric #-}

module Data.Datamining.Clustering.SOSInternal where

import Prelude hiding (lookup, null)

import Control.DeepSeq (NFData)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Data.Datamining.Clustering.Classifier(Classifier(..))
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

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
exponential :: (Floating a, Integral t) => a -> a -> t -> a
exponential r0 d t = r0 * exp (-d*t')
  where t' = fromIntegral t

-- | A Simplified Self-Organising Map (SOS).
--   @x@ is the type of the learning rate and the difference metric.
--   @t@ is the type of the counter.
--   @k@ is the type of the model indices.
--   @p@ is the type of the input patterns and models.
data SOS t x k p = SOS
  {
    -- | Maps patterns and match counts to nodes.
    sMap :: M.Map k (p, t),
    -- | A function which determines the learning rate for a node.
    --   The input parameter indicates how many patterns (or pattern
    --   batches) have previously been presented to the classifier.
    --   Typically this is used to make the learning rate decay over
    --   time.
    --   The output is the learning rate for that node (the amount by
    --   which the node's model should be updated to match the target).
    --   The learning rate should be between zero and one.
    learningRate :: t -> x,
    -- | The maximum number of models this SOS can hold.
    maxSize :: Int,
    -- | The threshold that triggers creation of a new model.
    diffThreshold :: x,
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
    -- | Index for the next node to add to the SOS.
    nextIndex :: k
  } deriving (Generic, NFData)

makeSOS
  :: Bounded k
    => (t -> x) -> Int -> x -> (p -> p -> x) -> (p -> x -> p -> p)
      -> SOS t x k p
makeSOS lr n dt diff ms = SOS M.empty lr n dt diff ms minBound

-- | Returns true if the SOS has no models, false otherwise.
null  :: SOS t x k p -> Bool
null = M.null . sMap

-- | Extracts the current models from the SOS.
toMap :: SOS t x k p -> M.Map k p
toMap = M.map fst . sMap

-- | The current "time" (number of times the SOS has been trained).
time :: Num t => SOS t x k p -> t
time = sum . map snd . M.elems . sMap

counters :: SOS t x k p -> M.Map k t
counters = M.map snd . sMap

-- | Adds a new node to the SOS.
addNode
  :: (Num t, Enum k, Ord k)
    => p -> SOS t x k p -> SOS t x k p
addNode p s = if numModels' s >= maxSize s
                then error "SOS is full"
                else s { sMap=gm', nextIndex=succ k }
  where gm = sMap s
        k = nextIndex s
        gm' = M.insert k (p, 0) gm

-- | Removes a node from the SOS.
--   Deleted nodes are never re-used.
deleteNode :: Ord k => k -> SOS t x k p -> SOS t x k p
deleteNode k s = s { sMap=gm' }
  where gm = sMap s
        gm' = if M.member k gm
                then M.delete k gm
                else error "no such node"

incrementCounter :: (Num t, Ord k) => k -> SOS t x k p -> SOS t x k p
incrementCounter k s = s { sMap=gm' }
  where gm = sMap s
        gm' = if M.member k gm
                then M.adjust inc k gm
                else error "no such node"
        inc (p, t) = (p, t+1)

-- | Trains the specified node to better match a target.
--   Most users should use @'train'@, which automatically determines
--   the BMU and trains it.
trainNode
  :: (Num t, Ord k)
    => SOS t x k p -> k -> p -> SOS t x k p
trainNode s k target = s { sMap=gm' }
  where gm = sMap s
        gm' = M.adjust tweakModel k gm
        r = (learningRate s) (time s)
        tweakModel (p, t) = (makeSimilar s target r p, t)

leastUsefulNode :: Ord t => SOS t x k p -> k
leastUsefulNode s = if null s
                      then error "SOS has no nodes"
                      else fst . findMinValue . counters $ s

deleteLeastUsefulNode :: (Ord t, Ord k) => SOS t x k p -> SOS t x k p
deleteLeastUsefulNode s = deleteNode k s
  where k = leastUsefulNode s

addModel :: (Num t, Ord t, Enum k, Ord k) => p -> SOS t x k p -> SOS t x k p
addModel p s = addNode p s'
  where s' = if numModels' s >= maxSize s
                then deleteLeastUsefulNode s
                else s

-- | Finds the node whose model that best matches the pattern.
--   Returns the index of the node, and the difference between the
--   model and the pattern.
findBMU :: Ord x => SOS t x k p -> p -> (k, x)
findBMU s p = findMinValue . M.map (difference s p) $ toMap s

justTrain
  :: (Num t, Ord t, Ord x, Enum k, Ord k)
    => SOS t x k p -> p -> SOS t x k p
justTrain s p = incrementCounter bmu2 s2
  where s1 = if null s then addModel p s else s
        (bmu, diff) = findBMU s1 p
        s2 = if diff > diffThreshold s
               then addModel p s1
               else trainNode s1 bmu p
        (bmu2, _) = findBMU s2 p

numModels' :: SOS t x k p -> Int
numModels' = length . M.keys . sMap

instance
  (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => Classifier (SOS t) x k p where
  toList = M.toList . toMap
  numModels = numModels'
  models = M.elems . toMap
  differences s p = M.toList . M.map (difference s p) $ toMap s
  trainBatch s = foldl' justTrain s
  reportAndTrain s p = (bmu, ds, s')
    where ds = differences s p
          bmu = fst $ minimumBy (comparing snd) ds
          s' = trainNode s bmu $ p

findMinValue :: Ord a => M.Map k a -> (k, a)
findMinValue = minimumBy (comparing snd) . M.toList
