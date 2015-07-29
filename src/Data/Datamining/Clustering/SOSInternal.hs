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

import Prelude hiding (lookup)

import Control.DeepSeq (NFData)
import Data.List (minimumBy, foldl')
import Data.Ord (comparing)
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
    toMap :: M.Map k (p, t),
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

-- @'makeSOS' lr n dt diff ms@ creates a new SOS that does not (yet)
-- contain any models.
-- It will learn at the rate determined by the learning function @lr@,
-- and will be able to hold up to @n@ models.
-- It will create a new model based on a pattern presented to it when
-- (1) the SOS contains no models, or
-- (2) the difference between the pattern and the closest matching
-- model exceeds the threshold @dt@.
-- It will use the function @diff@ to measure the similarity between
-- an input pattern and a model.
-- It will use the function @ms@ to adjust models as needed to make
-- them more similar to input patterns.
makeSOS
  :: Bounded k
    => (t -> x) -> Int -> x -> (p -> p -> x) -> (p -> x -> p -> p)
      -> SOS t x k p
makeSOS lr n dt diff ms =
  if n <= 0
    then error "max size for SOS <= 0"
    else SOS M.empty lr n dt diff ms minBound

-- | Returns true if the SOS has no models, false otherwise.
isEmpty :: SOS t x k p -> Bool
isEmpty = M.null . toMap

-- | Returns the number of models the SOS currently contains.
numModels :: SOS t x k p -> Int
numModels = length . M.keys . toMap

-- | Returns a map from node ID to model.
modelMap :: SOS t x k p -> M.Map k p
modelMap = M.map fst . toMap

-- | Returns a map from node ID to counter (number of times the
--   node's model has been the closest match to an input pattern).
counterMap :: SOS t x k p -> M.Map k t
counterMap = M.map snd . toMap

-- | Returns the current models.
models :: SOS t x k p -> [p]
models = map fst . M.elems . toMap

-- | Returns the current counters (number of times the
--   node's model has been the closest match to an input pattern).
counters :: SOS t x k p -> [t]
counters = map snd . M.elems . toMap

-- | The current "time" (number of times the SOS has been trained).
time :: Num t => SOS t x k p -> t
time = sum . map snd . M.elems . toMap

-- | Adds a new node to the SOS.
addNode
  :: (Num t, Enum k, Ord k)
    => p -> SOS t x k p -> (k, SOS t x k p)
addNode p s = if numModels s >= maxSize s
                then error "SOS is full"
                else (k, s { toMap=gm', nextIndex=succ k })
  where gm = toMap s
        k = nextIndex s
        gm' = M.insert k (p, 0) gm

-- | Removes a node from the SOS.
--   Deleted nodes are never re-used.
deleteNode :: Ord k => k -> SOS t x k p -> SOS t x k p
deleteNode k s = s { toMap=gm' }
  where gm = toMap s
        gm' = if M.member k gm
                then M.delete k gm
                else error "no such node"

incrementCounter :: (Num t, Ord k) => k -> SOS t x k p -> SOS t x k p
incrementCounter k s = s { toMap=gm' }
  where gm = toMap s
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
trainNode s k target = s { toMap=gm' }
  where gm = toMap s
        gm' = M.adjust tweakModel k gm
        r = (learningRate s) (time s)
        tweakModel (p, t) = (makeSimilar s target r p, t)

leastUsefulNode :: Ord t => SOS t x k p -> k
leastUsefulNode s = if isEmpty s
                      then error "SOS has no nodes"
                      else fst . minimumBy (comparing (snd . snd))
                             . M.toList . toMap $ s

deleteLeastUsefulNode :: (Ord t, Ord k) => SOS t x k p -> SOS t x k p
deleteLeastUsefulNode s = deleteNode k s
  where k = leastUsefulNode s

addModel
  :: (Num t, Ord t, Enum k, Ord k)
    => p -> SOS t x k p -> (k, SOS t x k p)
addModel p s = addNode p s'
  where s' = if numModels s >= maxSize s
                then deleteLeastUsefulNode s
                else s

reportAddModel
  :: (Num t, Ord t, Num x, Enum k, Ord k)
    => SOS t x k p -> p -> (k, x, [(k, x)], SOS t x k p)
reportAddModel s p = (k, 0, [(k, 0)], s'')
  where (k, s') = addModel p s
        s'' = incrementCounter k s'

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   If necessary, it will create a new node and model.
--   Returns the ID of the node with the best matching model,
--   the difference between the best matching model and the pattern,
--   the differences between the input and each model in the SOS,
--   and the (possibly updated) SOS.
classify
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SOS t x k p -> p -> (k, x, [(k, x)], SOS t x k p)
classify s p
  | isEmpty s                 = reportAddModel s p
  | bmuDiff > diffThreshold s = reportAddModel s p
  | otherwise                 = (bmu, bmuDiff, diffs, s')
  where (bmu, bmuDiff) = minimumBy (comparing snd) diffs
        diffs = M.toList . M.map (difference s p) . M.map fst
                    . toMap $ s
        s' = incrementCounter bmu s

-- | @'train' s p@ identifies the model in @s@ that most closely
--   matches @p@, and updates it to be a somewhat better match.
train
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SOS t x k p -> p -> SOS t x k p
train s p = trainNode s' bmu p
  where (bmu, _, _, s') = classify s p

-- | For each pattern @p@ in @ps@, @'trainBatch' s ps@ identifies the
--   model in @s@ that most closely matches @p@,
--   and updates it to be a somewhat better match.
trainBatch
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SOS t x k p -> [p] -> SOS t x k p
trainBatch = foldl' train
