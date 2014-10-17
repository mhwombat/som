------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2014
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
import Data.Datamining.Pattern (Pattern(..))
import Data.Datamining.Clustering.Classifier(Classifier(..))
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | A function used to adjust the models in a classifier.
class LearningFunction f where
  type LearningRate f
  -- | @'rate' f t@ returns the learning rate for a node.
  --   The parameter @f@ is the learning function.
  --   The parameter @t@ indicates how many patterns (or pattern
  --   batches) have previously been presented to the classifier.
  --   Typically this is used to make the learning rate decay over time.
  --   The output is the learning rate for that node (the amount by
  --   which the node's model should be updated to match the target).
  --   The learning rate should be between zero and one.
  rate :: f -> LearningRate f -> LearningRate f

-- | A typical learning function for classifiers.
--   @'Gaussian' r0 rf tf@ returns a gaussian function. At time zero,
--   the learning rate is @r0@. Over time the learning rate tapers off,
--   until at time @tf@, the learning rate is @rf@. Normally the
--   parameters should be chosen such that:
--
--   * 0 < rf << r0 < 1
--
--   * 0 < tf
--
--   where << means "is much smaller than" (not the Haskell @<<@
--   operator!)
data Gaussian a = Gaussian a a a
  deriving (Eq, Show, Generic)

instance (Floating a, Fractional a, Num a)
    => LearningFunction (Gaussian a) where
  type LearningRate (Gaussian a) = a
  rate (Gaussian r0 rf tf) t = r0 * ((rf/r0)**(t/tf))

-- | A Simplified Self-Organising Map (SSOM).
data SSOM f t k p = SSOM
  {
    -- | Maps patterns to nodes.
    sMap :: M.Map k p,
    -- | The function used to update the nodes.
    learningFunction :: f,
    -- | A counter used as a "time" parameter.
    --   If you create the SSOM with a counter value @0@, and don't
    --   directly modify it, then the counter will represent the number
    --   of patterns that this SSOM has classified.
    counter :: t
  } deriving (Eq, Show, Generic)

-- | Extracts the current models from the SSOM.
--   A synonym for @'sMap'@.
toMap :: SSOM f t k p -> M.Map k p
toMap = sMap

-- | Trains the specified node to better match a target.
--   Most users should use @train@, which automatically determines
--   the BMU and trains it.
trainNode
  :: (Pattern p, LearningFunction f, Metric p ~ LearningRate f,
    Num (LearningRate f), Ord k, Integral t)
      => SSOM f t k p -> k -> p -> SSOM f t k p
trainNode s k target = s { sMap=gm' }
  where gm = sMap s
        gm' = M.adjust (makeSimilar target r) k gm
        r = rate (learningFunction s) (fromIntegral $ counter s)

incrementCounter :: Num t => SSOM f t k p -> SSOM f t k p
incrementCounter s = s { counter=counter s + 1}

justTrain
  :: (Ord (Metric p), Pattern p, LearningFunction f,
    Metric p ~ LearningRate f, Num (LearningRate f), Ord k, Integral t)
      => SSOM f t k p -> p -> SSOM f t k p
justTrain s p = trainNode s bmu p
  where ds = M.toList . M.map (p `difference`) . toMap $ s
        bmu = f ds
        f [] = error "SSOM has no models"
        f xs = fst $ minimumBy (comparing snd) xs

instance
  (Pattern p, Ord (Metric p), LearningFunction f,
    Metric p ~ LearningRate f, Num (LearningRate f), Ord k, Integral t)
      => Classifier (SSOM f t) k p where
  toList = M.toList . toMap
  -- TODO: If the # of models is fixed, make more efficient
  numModels = length . M.keys . sMap
  models = M.elems . toMap
  differences s p = M.toList . M.map (p `difference`) $ toMap s
  trainBatch s = incrementCounter . foldl' justTrain s
  reportAndTrain s p = (bmu, ds, s')
    where ds = differences s p
          bmu = fst $ minimumBy (comparing snd) ds
          s' = incrementCounter . trainNode s bmu $ p
