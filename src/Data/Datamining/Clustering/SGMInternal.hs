------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SGM@ internals. Most developers should
-- use @SGM@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, DeriveAnyClass, DeriveGeneric #-}

module Data.Datamining.Clustering.SGMInternal where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData)
import Data.List (minimumBy, sortBy, foldl')
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

-- | A Simplified Self-Organising Map (SGM).
--   @t@ is the type of the counter.
--   @x@ is the type of the learning rate and the difference metric.
--   @k@ is the type of the model indices.
--   @p@ is the type of the input patterns and models.
data SGM t x k p = SGM
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
    -- | The maximum number of models this SGM can hold.
    maxSize :: Int,
    -- | The threshold that triggers creation of a new model.
    diffThreshold :: x,
    -- | Delete existing models to make room for new ones? The least
    --   useful (least frequently matched) models will be deleted first.
    allowDeletion :: Bool,
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
    -- | Index for the next node to add to the SGM.
    nextIndex :: k
  } deriving (Generic, NFData)

-- @'makeSGM' lr n dt diff ms@ creates a new SGM that does not (yet)
-- contain any models.
-- It will learn at the rate determined by the learning function @lr@,
-- and will be able to hold up to @n@ models.
-- It will create a new model based on a pattern presented to it when
-- (1) the SGM contains no models, or
-- (2) the difference between the pattern and the closest matching
-- model exceeds the threshold @dt@.
-- It will use the function @diff@ to measure the similarity between
-- an input pattern and a model.
-- It will use the function @ms@ to adjust models as needed to make
-- them more similar to input patterns.
makeSGM
  :: Bounded k
    => (t -> x) -> Int -> x -> Bool -> (p -> p -> x)
      -> (p -> x -> p -> p) -> SGM t x k p
makeSGM lr n dt ad diff ms =
  if n <= 0
    then error "max size for SGM <= 0"
    else SGM M.empty lr n dt ad diff ms minBound

-- | Returns true if the SGM has no models, false otherwise.
isEmpty :: SGM t x k p -> Bool
isEmpty = M.null . toMap

-- | Returns the number of models the SGM currently contains.
numModels :: SGM t x k p -> Int
numModels = M.size . toMap

-- | Returns a map from node ID to model.
modelMap :: SGM t x k p -> M.Map k p
modelMap = M.map fst . toMap

-- | Returns a map from node ID to counter (number of times the
--   node's model has been the closest match to an input pattern).
counterMap :: SGM t x k p -> M.Map k t
counterMap = M.map snd . toMap

-- | Returns the model at a specified node.
modelAt :: Ord k => SGM t x k p -> k -> p
modelAt s k = (modelMap s) M.! k

-- | Returns the current labels.
labels :: SGM t x k p -> [k]
labels = M.keys . toMap

-- | Returns the current models.
models :: SGM t x k p -> [p]
models = map fst . M.elems . toMap

-- | Returns the current counters (number of times the
--   node's model has been the closest match to an input pattern).
counters :: SGM t x k p -> [t]
counters = map snd . M.elems . toMap

-- | The current "time" (number of times the SGM has been trained).
time :: Num t => SGM t x k p -> t
time = sum . map snd . M.elems . toMap

-- | Adds a new node to the SGM.
addNode
  :: (Num t, Enum k, Ord k)
    => p -> SGM t x k p -> SGM t x k p
addNode p s = if numModels s >= maxSize s
                then error "SGM is full"
                else s { toMap=gm', nextIndex=succ k }
  where gm = toMap s
        k = nextIndex s
        gm' = M.insert k (p, 0) gm

-- | Removes a node from the SGM.
--   Deleted nodes are never re-used.
deleteNode :: Ord k => k -> SGM t x k p -> SGM t x k p
deleteNode k s = s { toMap=gm' }
  where gm = toMap s
        gm' = if M.member k gm
                then M.delete k gm
                else error "no such node"

incrementCounter :: (Num t, Ord k) => k -> SGM t x k p -> SGM t x k p
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
    => SGM t x k p -> k -> p -> SGM t x k p
trainNode s k target = s { toMap=gm' }
  where gm = toMap s
        gm' = M.adjust tweakModel k gm
        r = (learningRate s) (time s)
        tweakModel (p, t) = (makeSimilar s target r p, t)

leastUsefulNode :: Ord t => SGM t x k p -> k
leastUsefulNode s = if isEmpty s
                      then error "SGM has no nodes"
                      else fst . minimumBy (comparing (snd . snd))
                             . M.toList . toMap $ s

deleteLeastUsefulNode :: (Ord t, Ord k) => SGM t x k p -> SGM t x k p
deleteLeastUsefulNode s = deleteNode k s
  where k = leastUsefulNode s

addModel
  :: (Num t, Ord t, Enum k, Ord k)
    => p -> SGM t x k p -> SGM t x k p
addModel p s = addNode p s'
  where s' = if numModels s >= maxSize s
                then deleteLeastUsefulNode s
                else s

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   It will not make any changes to the classifier.
--   Returns the ID of the node with the best matching model,
--   the difference between the best matching model and the pattern,
--   and the SGM labels paired with the model and the difference
--   between the input and the corresponding model.
--   The final paired list is sorted in decreasing order of similarity.
classify
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> (k, x, M.Map k (p, x))
classify s p = (bmu, bmuDiff, report)
  where sFull = s { maxSize = numModels s, allowDeletion = False }
          -- don't allow any changes!
        (bmu, bmuDiff, report, _) = classify' sFull p


-- NOTE: This function may create a new model, but it does not modify
-- existing models.
classify'
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> (k, x, M.Map k (p, x), SGM t x k p)
classify' s p
  | isEmpty s                 = classify' (addModel p s) p
  | bmuDiff > diffThreshold s
      && (numModels s < maxSize s || allowDeletion s)
                              = classify' (addModel p s) p
  | otherwise                 = (bmu, bmuDiff, report, s')
  where report
          = M.map (\p0 -> (p0, difference s p p0)) . modelMap $ s
        (bmu, bmuDiff)
          = head . sortBy matchOrder . map (\(k, (_, x)) -> (k, x))
              . M.toList $ report
        s' = incrementCounter bmu s

-- We want the model with the lowest difference from the input pattern.
-- If two models have the same difference, return the model that was
-- created earlier (has the lower label #).
matchOrder :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
matchOrder (a, b) (c, d) = compare (b, a) (d, c)

-- | @'trainAndClassify' s p@ identifies the model in @s@ that most
--   closely matches @p@, and updates it to be a somewhat better match.
--   If necessary, it will create a new node and model.
--   Returns the ID of the node with the best matching model,
--   the difference between the best matching model and the pattern,
--   the differences between the input and each model in the SGM,
--   and the updated SGM.
trainAndClassify
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> (k, x, M.Map k (p, x), SGM t x k p)
trainAndClassify s p = (bmu, bmuDiff, report, s3)
  where (bmu, bmuDiff, report, s2) = classify' s p
        s3 = trainNode s2 bmu p

-- | @'train' s p@ identifies the model in @s@ that most closely
--   matches @p@, and updates it to be a somewhat better match.
--   If necessary, it will create a new node and model.
train
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> SGM t x k p
train s p = s'
  where (_, _, _, s') = trainAndClassify s p

-- | For each pattern @p@ in @ps@, @'trainBatch' s ps@ identifies the
--   model in @s@ that most closely matches @p@,
--   and updates it to be a somewhat better match.
trainBatch
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> [p] -> SGM t x k p
trainBatch = foldl' train

