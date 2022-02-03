------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGM4Internal
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
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
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Datamining.Clustering.SGM4Internal where

import           Prelude         hiding (filter, lookup)

import           Control.DeepSeq (NFData)
import           Data.List       (foldl', minimumBy, (\\))
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
-- import           Data.Ratio      ((%))
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

class Adjuster a where
  type TimeType a
  type MetricType a
  type PatternType a
  -- | A function which determines the current learning rate.
  --   The input parameter indicates how many patterns (or pattern
  --   batches) have previously been presented to the classifier.
  --   Typically this is used to make the learning rate decay over
  --   time.
  --   The output is the learning rate for that node (the amount by
  --   which the node's model should be updated to match the target).
  --   The learning rate should be between zero and one.
  learningRate :: a -> TimeType a -> MetricType a
  -- | A function which compares two patterns and returns a
  --   /non-negative/ number representing how different the patterns
  --   are.
  --   A result of @0@ indicates that the patterns are identical.
  difference :: a -> PatternType a -> PatternType a -> MetricType a
  -- | A function which updates models.
  --   For example, if this function returns @f@, then
  --   @f target amount pattern@ returns a modified copy of @pattern@
  --   that is more similar to @target@ than @pattern@ is.
  --   The magnitude of the adjustment is controlled by the @amount@
  --   parameter, which should be a number between 0 and 1.
  --   Larger values for @amount@ permit greater adjustments.
  --   If @amount@=1, the result should be identical to the @target@.
  --   If @amount@=0, the result should be the unmodified @pattern@.
  makeSimilar :: a -> PatternType a -> MetricType a -> PatternType a -> PatternType a

-- | A Simplified Self-Organising Map.
--   @a@ is the type of the adjuster.
--   @t@ is the type of the counter.
--   @x@ is the type of the learning rate and the difference metric.
--   @k@ is the type of the model indices.
--   @p@ is the type of the input patterns and models.
data SGM a t k p = SGM
  {
    -- | Maps patterns and match counts to nodes.
    toMap     :: M.Map k (p, t),
    -- | Pattern adjustment and learning functions.
    adjuster  :: a,
    -- | The maximum number of models this SGM can hold.
    capacity  :: Int,
    -- | Index for the next node to add to the SGM.
    nextIndex :: k
  } deriving (Generic, NFData)

deriving instance (Eq a, Eq t, Eq k, Eq p) => Eq (SGM a t k p)

deriving instance (Read a, Read t, Read k, Ord k, Read p) => Read (SGM a t k p)

deriving instance (Show a, Show t, Show k, Show p) => Show (SGM a t k p)

-- | @'makeSGM' a n@ creates a new SGM of size @n@ that does not (yet)
--   contain any models, with learning controlled by @a@.
--   It will create a new model based on a pattern presented to it when
--   the SGM is not at capacity, or a less useful model can be replaced.
makeSGM :: (Adjuster a, Bounded k) => a -> Int -> SGM a t k p
makeSGM a n =
  if n <= 0
    then error "max size for SGM <= 0"
    else SGM M.empty a n minBound

-- | Returns true if the SGM has no models, false otherwise.
isEmpty :: SGM a t k p -> Bool
isEmpty = M.null . toMap

-- | Returns the number of models the SGM currently contains.
size :: SGM a t k p -> Int
size = M.size . toMap

-- | Returns a map from node ID to model.
modelMap :: SGM a t k p -> M.Map k p
modelMap = M.map fst . toMap

-- | Returns a map from node ID to counter (number of times the
--   node's model has been the closest match to an input pattern).
counterMap :: Adjuster a => SGM a t k p -> M.Map k t
counterMap = M.map snd . toMap

-- | Returns the model at a specified node.
modelAt :: (Adjuster a, Ord k) => SGM a t k p -> k -> p
modelAt s k = modelMap s M.! k

-- | Returns the match counter for a specified node.
counterAt :: (Adjuster a, Ord k) => SGM a t k p -> k -> t
counterAt s k = counterMap s M.! k

-- | Returns the current labels.
labels :: SGM a t k p -> [k]
labels = M.keys . toMap

-- | The current "time" (number of times the SGM has been trained).
time :: (Adjuster a, Num t, TimeType a ~ t) => SGM a t k p -> t
time = sum . map snd . M.elems . toMap

-- | Adds a new node to the SGM.
addNode
  :: (Adjuster a, Num t, Bounded k, Enum k, Ord k)
  => SGM a t k p -> p -> SGM a t k p
addNode s = addNodeAt s (nextIndex s)

addNodeAt
  :: (Adjuster a, Num t, Bounded k, Enum k, Ord k)
  => SGM a t k p -> k -> p -> SGM a t k p
addNodeAt s k p
  | atCapacity s   = error "SGM is full"
  | s `hasLabel` k = error "label already exists"
  | otherwise      = s { toMap=gm', nextIndex=kNext }
  where gm = toMap s
        gm' = M.insert k (p, 0) gm
        -- kNext = succ . maximum . M.keys $ gm'
        allPossibleIndices = enumFromTo minBound maxBound
        usedIndices = M.keys gm'
        availableIndices = allPossibleIndices \\ usedIndices
        kNext = head availableIndices

-- | Increments the match counter.
incrementCounter
  :: (Adjuster a, Num t, Ord k)
  => k -> SGM a t k p -> SGM a t k p
incrementCounter k s = s { toMap=gm' }
  where gm = toMap s
        gm' | M.member k gm = M.adjust inc k gm
            | otherwise     = error "no such node"
        inc (p, t) = (p, t+1)

-- | Trains the specified node to better match a target.
--   Most users should use @'train'@, which automatically determines
--   the BMU and trains it.
trainNode
  :: (Adjuster a, Num t, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> k -> p -> SGM a t k p
trainNode s k target = s { toMap=gm' }
  where gm = toMap s
        gm' = M.adjust tweakModel k gm
        r = learningRate (adjuster s) $ (time s)
        tweakModel (p, t) = (ms target r p, t)
        ms = makeSimilar (adjuster s)

hasLabel :: Ord k => SGM a t k p -> k -> Bool
hasLabel s k = M.member k . toMap $ s

imprint
  :: (Adjuster a, Num t, Ord t,
     Fractional (MetricType a), Num (MetricType a), Ord (MetricType a),
     Bounded k, Enum k, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> k -> p -> SGM a t k p
imprint s k p
  | s `hasLabel` k = trainNode s k p
  | atCapacity s   = train s p
  | otherwise      = addNodeAt s k p

imprintBatch
  :: (Adjuster a, Num t, Ord t,
     Fractional (MetricType a), Num (MetricType a), Ord (MetricType a),
     Bounded k, Enum k, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> [(k, p)] -> SGM a t k p
imprintBatch = foldl' imprintOne
  where imprintOne s' (k, p) = imprint s' k p

-- | Calculates the difference between all pairs of non-identical
--   labels in the SGM.
modelDiffs
  :: (Adjuster a, Eq k, Ord k, PatternType a ~ p)
  => SGM a t k p -> [((k, k), MetricType a)]
modelDiffs s = map f $ labelPairs s
  where f (k, k') = ( (k, k'), diff (s `modelAt` k) (s `modelAt` k') )
        diff = difference (adjuster s)

-- | Generates all pairs of non-identical labels in the SGM.
labelPairs :: Eq k => SGM a t k p -> [(k, k)]
labelPairs s = concatMap (labelPairs' s) $ labels s

-- | Pairs a node label with all labels except itself.
labelPairs' :: Eq k => SGM a t k p -> k -> [(k, k)]
labelPairs' s k = map (\k' -> (k, k')) $ labels s \\ [k]

-- | Returns the labels of the two most similar models, and the
--   difference between them.
twoMostSimilar
  :: (Adjuster a, Ord (MetricType a), Eq k, Ord k, PatternType a ~ p)
  => SGM a t k p -> (k, k, MetricType a)
twoMostSimilar s
  | size s < 2 = error "there aren't two models to merge"
  | otherwise  = (k, k', x)
  where ((k, k'), x) = minimumBy (comparing snd) $ modelDiffs s

-- | Deletes the least used (least matched) model in a pair,
--   and returns its label (now available) and the updated SGM.
--   TODO: Modify the other model to make it slightly more similar to
--   the one that was deleted?
mergeModels
  :: (Adjuster a, Num t, Ord t, Ord k)
  => SGM a t k p -> k -> k -> (k, SGM a t k p)
mergeModels s k1 k2
  | not (M.member k1 gm) = error "no such node 1"
  | not (M.member k2 gm) = error "no such node 2"
  | otherwise          = (kDelete, s { toMap = gm' })
  where c1 = s `counterAt` k1
        c2 = s `counterAt` k2
        (kKeep, kDelete) | c1 >= c2   = (k1, k2)
                         | otherwise = (k2, k1)
        gm = toMap s
        gm' = M.adjust f kKeep $ M.delete kDelete gm
        f (p, _) = (p, c1 + c2)

-- | Returns True if the SOM is full; returns False if it can add one
--   or more models.
atCapacity :: SGM a t k p -> Bool
atCapacity s = size s == capacity s

-- | @'consolidate' s@ finds the two most similar models, and combines
--   them. This can be used to free up more space for learning. It
--   returns the index of the newly free node, and the updated SGM.
consolidate
  :: (Adjuster a, Num t, Ord t, Ord (MetricType a), Ord k,
     PatternType a ~ p)
  => SGM a t k p -> (k, SGM a t k p)
consolidate s = (k3, s2)
  where (k1, k2, _) = twoMostSimilar s
        (k3, s2) = mergeModels s k1 k2

consolidateAndAdd
  :: (Adjuster a, Num t, Ord t,
     Ord (MetricType a), Bounded k, Enum k, Ord k, PatternType a ~ p)
  => SGM a t k p -> p -> SGM a t k p
consolidateAndAdd s = addNode s'
  where (_, s') = consolidate s

-- | Set the model for a node.
--   Useful when merging two models and replacing one.
setModel
  :: (Adjuster a, Num t, Ord k)
  => SGM a t k p -> k -> p -> SGM a t k p
setModel s k p
  | M.member k gm = error "node already exists"
  | otherwise     = s { toMap = gm' }
  where gm = toMap s
        gm' = M.insert k (p, 0) gm

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
  :: (Adjuster a, Num t, Ord t,
     Num (MetricType a), Ord (MetricType a),
     Enum k, Ord k, PatternType a ~ p)
  => SGM a t k p -> p
    -> (k, (MetricType a), M.Map k (p, MetricType a))
classify s p
  | isEmpty s = error "SGM has no models"
  | otherwise = (bmu, bmuDiff, report)
  where report
          = M.map (\p0 -> (p0, diff p p0)) . modelMap $ s
        diff = difference (adjuster s)
        (bmu, bmuDiff)
          = minimumBy matchOrder . map (\(k, (_, x)) -> (k, x))
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
  :: (Adjuster a, Num t, Ord t,
     Fractional (MetricType a), Num (MetricType a), Ord (MetricType a),
     Bounded k, Enum k, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> p -> (k, (MetricType a), M.Map k (p, MetricType a), SGM a t k p)
trainAndClassify s p = trainAndClassify' s' p
  where s' | size s > 1 && bmuDiff == 0        = s
           | atCapacity s && capacity s == 1   = s
           | size s < 2                      = addNode s p
           | atCapacity s && bmuDiff >= cutoff = consolidateAndAdd s p
           | atCapacity s                    = s
           | otherwise                       = addNode s p
        (_, bmuDiff, _) = classify s p
        (_, _, cutoff) = twoMostSimilar s

-- | Internal method.
-- NOTE: This function will adjust the model and update the match
-- for the BMU.
trainAndClassify'
  :: (Adjuster a, Num t, Ord t,
     Num (MetricType a), Ord (MetricType a),
     Enum k, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> p
    -> (k, (MetricType a), M.Map k (p, MetricType a), SGM a t k p)
trainAndClassify' s p = (bmu2, bmuDiff, report, s3)
  where (bmu, bmuDiff, _) = classify s p
        s2 = incrementCounter bmu s
        s3 = trainNode s2 bmu p
        (bmu2, _, report) = classify s3 p

-- | @'train' s p@ identifies the model in @s@ that most closely
--   matches @p@, and updates it to be a somewhat better match.
--   If necessary, it will create a new node and model.
train
  :: (Adjuster a, Num t, Ord t,
     Fractional (MetricType a), Num (MetricType a), Ord (MetricType a),
     Bounded k, Enum k, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> p -> SGM a t k p
train s p = s'
  where (_, _, _, s') = trainAndClassify s p

-- | For each pattern @p@ in @ps@, @'trainBatch' s ps@ identifies the
--   model in @s@ that most closely matches @p@,
--   and updates it to be a somewhat better match.
trainBatch
  :: (Adjuster a, Num t, Ord t,
     Fractional (MetricType a), Num (MetricType a), Ord (MetricType a),
     Bounded k, Enum k, Ord k, TimeType a ~ t, PatternType a ~ p)
  => SGM a t k p -> [p] -> SGM a t k p
trainBatch = foldl' train

-- | Same as @'size'@.
numModels :: SGM a t k p -> Int
numModels = size

-- | Same as @'capacity'@.
maxSize :: SGM a t k p -> Int
maxSize = capacity

-- | Returns a copy of the SOM containing only models that satisfy the
--   predicate.
filter :: Adjuster a => (p -> Bool) -> SGM a t k p -> SGM a t k p
filter f s = s { toMap = pm' }
  where pm = toMap s
        pm' = M.filter (\(p, _) -> f p) pm
