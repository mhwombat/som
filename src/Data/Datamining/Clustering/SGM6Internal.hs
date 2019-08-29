------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGM6Internal
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SGM@ internals. Most developers should
-- use @SGM@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Datamining.Clustering.SGM6Internal where

import           Prelude         hiding (filter, lookup)

import           Control.DeepSeq (NFData)
import           Data.List       (foldl', sortBy)
import qualified Data.Map.Strict as M
import           GHC.Generics    (Generic)

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
exponential :: (Floating x, Integral t) => x -> x -> t -> x
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
    toMap        :: M.Map k (p, t),
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
    capacity     :: Int,
    -- | A function which compares two patterns and returns a
    --   /non-negative/ number representing how different the patterns
    --   are.
    --   A result of @0@ indicates that the patterns are identical.
    difference   :: p -> p -> x,
    -- | A function which updates models.
    --   For example, if this function is @f@, then
    --   @f target amount pattern@ returns a modified copy of @pattern@
    --   that is more similar to @target@ than @pattern@ is.
    --   The magnitude of the adjustment is controlled by the @amount@
    --   parameter, which should be a number between 0 and 1.
    --   Larger values for @amount@ permit greater adjustments.
    --   If @amount@=1, the result should be identical to the @target@.
    --   If @amount@=0, the result should be the unmodified @pattern@.
    makeSimilar  :: p -> x -> p -> p,
    -- | Index for the next node to add to the SGM.
    nextIndex    :: k
  } deriving (Generic, NFData)

-- | @'makeSGM' lr n diff ms@ creates a new SGM that does not (yet)
--   contain any models.
--   It will learn at the rate determined by the learning function @lr@,
--   and will be able to hold up to @n@ models.
--   It will create a new model based on a pattern presented to it when
--   the SGM is not at capacity, or a less useful model can be replaced.
--   It will use the function @diff@ to measure the similarity between
--   an input pattern and a model.
--   It will use the function @ms@ to adjust models as needed to make
--   them more similar to input patterns.
makeSGM
  :: Bounded k
    => (t -> x) -> Int -> (p -> p -> x) -> (p -> x -> p -> p) -> SGM t x k p
makeSGM lr n diff ms =
  if n <= 0
    then error "max size for SGM <= 0"
    else SGM M.empty lr n diff ms minBound

-- | Returns true if the SGM has no models, false otherwise.
isEmpty :: SGM t x k p -> Bool
isEmpty = M.null . toMap

-- | Returns the number of models the SGM currently contains.
size :: SGM t x k p -> Int
size = M.size . toMap

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

-- | Returns the match counter for a specified node.
counterAt :: Ord k => SGM t x k p -> k -> t
counterAt s k = (counterMap s) M.! k

-- | Returns the current labels.
labels :: SGM t x k p -> [k]
labels = M.keys . toMap

-- | The current "time" (number of times the SGM has been trained).
time :: Num t => SGM t x k p -> t
time = sum . map snd . M.elems . toMap

-- | Adds a new node to the SGM.
addNode
  :: (Num t, Enum k, Ord k)
    => p -> SGM t x k p -> SGM t x k p
addNode p s = if atCapacity s
                then error "SGM is full"
                else s { toMap=gm', nextIndex=succ k }
  where gm = toMap s
        k = nextIndex s
        gm' = M.insert k (p, 0) gm

-- | Increments the match counter.
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

-- | Returns True if the SOM is full; returns False if it can add one
--   or more models.
atCapacity :: SGM t x k p -> Bool
atCapacity s = size s == capacity s

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   It will not make any changes to the classifier.
--   (I.e., it will not change the models or match counts.)
--   Returns the ID of the node with the best matching model,
--   the difference between the best matching model and the pattern,
--   and the SGM labels paired with the model and the difference
--   between the input and the corresponding model.
--   The final paired list is sorted in decreasing order of similarity.
classify
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> (k, x, M.Map k (p, x))
classify s p
  | isEmpty s = error "SGM has no models"
  | otherwise = (bmu, bmuDiff, report)
  where report
          = M.map (\p0 -> (p0, difference s p p0)) . modelMap $ s
        (bmu, bmuDiff)
          = head . sortBy matchOrder . map (\(k, (_, x)) -> (k, x))
              . M.toList $ report

-- | Order models by ascending difference from the input pattern,
--   then by creation order (label number).
matchOrder :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
matchOrder (a, b) (c, d) = compare (b, a) (d, c)

-- | @'trainAndClassify' s p@ identifies the model in @s@ that most
--   closely matches @p@, and updates it to be a somewhat better match.
--   If necessary, it will create a new node and model.
--   Returns the ID of the node with the best matching model,
--   the difference between the pattern and the best matching model
--   in the original SGM (before training or adding a new model),
--   the differences between the pattern and each model in the updated
--   SGM,
--   and the updated SGM.
trainAndClassify
  :: (Num t, Ord t, Fractional x, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> (k, x, M.Map k (p, x), SGM t x k p)
trainAndClassify s p = trainAndClassify' s' p
  where s' | atCapacity s = s
           | isEmpty s    = addNode p s
           | bmuDiff == 0  = s
           | otherwise    = addNode p s
        (_, bmuDiff, _) = classify s p

-- | Internal method.
-- NOTE: This function will adjust the model and update the match
-- for the BMU.
trainAndClassify'
  :: (Num t, Ord t, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> (k, x, M.Map k (p, x), SGM t x k p)
trainAndClassify' s p = (bmu2, bmuDiff, report, s3)
  where (bmu, bmuDiff, _) = classify s p
        s2 = incrementCounter bmu s
        s3 = trainNode s2 bmu p
        (bmu2, _, report) = classify s3 p

-- | @'train' s p@ identifies the model in @s@ that most closely
--   matches @p@, and updates it to be a somewhat better match.
--   If necessary, it will create a new node and model.
train
  :: (Num t, Ord t, Fractional x, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> p -> SGM t x k p
train s p = s'
  where (_, _, _, s') = trainAndClassify s p

-- | For each pattern @p@ in @ps@, @'trainBatch' s ps@ identifies the
--   model in @s@ that most closely matches @p@,
--   and updates it to be a somewhat better match.
trainBatch
  :: (Num t, Ord t, Fractional x, Num x, Ord x, Enum k, Ord k)
    => SGM t x k p -> [p] -> SGM t x k p
trainBatch = foldl' train

-- | Same as @'size'@.
numModels :: SGM t x k p -> Int
numModels = size

-- | Same as @'capacity'@.
maxSize :: SGM t x k p -> Int
maxSize = capacity

-- | Returns a copy of the SOM containing only models that satisfy the
--   predicate.
filter :: (p -> Bool) -> SGM t x k p -> SGM t x k p
filter f s = s { toMap = pm' }
  where pm = toMap s
        pm' = M.filter (\(p, _) -> f p) pm

