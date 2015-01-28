------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SSOMQC
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
    FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}

module Data.Datamining.Clustering.SSOMQC
  (
    test
  ) where

import Data.Datamining.Pattern (Pattern, Metric, difference,
  euclideanDistanceSquared, makeSimilar)
import Data.Datamining.Clustering.Classifier(classify,
  classifyAndTrain, reportAndTrain, differences, diffAndTrain, models,
  train, trainBatch)
import Data.Datamining.Clustering.SSOMInternal
import qualified Data.Map.Strict as M

import Control.Applicative
import Data.Function (on)
import Data.List (sort)
import System.Random (Random)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Gen, Arbitrary, arbitrary, choose,
  Property, property, sized, suchThat, vectorOf)

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

instance
  (Random a, Num a, Ord a, Arbitrary a)
  => Arbitrary (Exponential a) where
  arbitrary = do
    r0 <- choose (0,1)
    d <- positive
    return $ Exponential r0 d

prop_Exponential_starts_at_r0
  :: Exponential Double -> Property
prop_Exponential_starts_at_r0 f@(Exponential r0 _)
  = property $ abs (rate f 0 - r0) < 0.01

prop_Exponential_ge_0
  :: Exponential Double -> Double -> Property
prop_Exponential_ge_0 f t
  = property $ rate f t' >= 0
  where t' = abs t

newtype TestPattern = MkPattern Double deriving Show

instance Eq TestPattern where
  (==) = (==) `on` toDouble

instance Ord TestPattern where
  compare = compare `on` toDouble

instance Pattern TestPattern where
  type Metric TestPattern = Double
  difference (MkPattern a) (MkPattern b) = abs (a - b)
  makeSimilar orig@(MkPattern a) r (MkPattern b)
    | r < 0     = error "Negative learning rate"
    | r > 1     = error "Learning rate > 1"
    | r == 1     = orig
    | otherwise = MkPattern (b + delta)
        where diff = a - b
              delta = r*diff

instance Arbitrary TestPattern where
  arbitrary = MkPattern <$> arbitrary

toDouble :: TestPattern -> Double
toDouble (MkPattern a) = a

absDiff :: [TestPattern] -> [TestPattern] -> Double
absDiff xs ys = euclideanDistanceSquared xs' ys'
  where xs' = map toDouble xs
        ys' = map toDouble ys

-- | A classifier and a training set. The training set will consist of
--   @j@ vectors of equal length, where @j@ is the number of patterns
--   the classifier can model. After running through the training set a
--   few times, the classifier should be very accurate at identifying
--   any of those @j@ vectors.
data SSOMandTargets = SSOMandTargets (SSOM (Exponential Double)
  Int Int TestPattern) [TestPattern]
    deriving (Eq, Show)

buildSSOMandTargets
  :: [TestPattern] -> Double -> Double -> [TestPattern] -> SSOMandTargets
buildSSOMandTargets ps r0 d targets =
  SSOMandTargets s targets
    where gm = M.fromList . zip [0..] $ ps
          s = SSOM gm (Exponential r0 d) 0

sizedSSOMandTargets :: Int -> Gen SSOMandTargets
sizedSSOMandTargets n = do
  let len = n + 1
  ps <- vectorOf len arbitrary
  r0 <- choose (0, 1)
  d <- positive
  targets <- vectorOf len arbitrary
  return $ buildSSOMandTargets ps r0 d targets

instance Arbitrary SSOMandTargets where
  arbitrary = sized sizedSSOMandTargets

prop_training_reduces_error :: SSOMandTargets -> Property
prop_training_reduces_error (SSOMandTargets s xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ toDouble x - toDouble (toMap s M.! bmu)
          errAfter = abs $ toDouble x - toDouble (toMap s' M.! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: SSOMandTargets -> Property
prop_classifyAndTrainEquiv (SSOMandTargets s ps) = property $
  bmu == s `classify` p && toMap s1 == toMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: SSOMandTargets -> Property
prop_diffAndTrainEquiv (SSOMandTargets s ps) = property $
  diffs == s `differences` p && toMap s1 == toMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNode s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNodeEquiv :: SSOMandTargets -> Property
prop_trainNodeEquiv (SSOMandTargets s ps) = property $
  toMap s1 == toMap s2
    where p = head ps
          s1 = trainNode s (classify s p) p
          s2 = train s p

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: SSOMandTargets -> Property
prop_batch_training_works (SSOMandTargets s xs) = property $
  classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent :: SSOMandTargets -> Property
prop_classification_is_consistent (SSOMandTargets s (x:_))
  = property $ bmu == bmu'
  where (bmu, _, s') = reportAndTrain s x
        (bmu', _, _) = reportAndTrain s' x
prop_classification_is_consistent _ = error "Should not happen"

-- | Same as SSOMandTargets, except that the initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern.
data SpecialSSOMandTargets = SpecialSSOMandTargets (SSOM
  (Exponential Double) Int Int TestPattern) [TestPattern]
    deriving (Eq, Show)

buildSpecialSSOMandTargets
  :: [TestPattern] -> Double -> Double -> [TestPattern]
    -> SpecialSSOMandTargets
buildSpecialSSOMandTargets ps r0 d targets =
  SpecialSSOMandTargets s targets
    where gm = M.fromList . zip [0..] $ ps
          s = SSOM gm (Exponential r0 d) 0

sizedSpecialSSOMandTargets :: Int -> Gen SpecialSSOMandTargets
sizedSpecialSSOMandTargets n = do
  let len = n + 1
  let ps = map MkPattern $ take len [0,100..]
  r0 <- choose (0, 1)
  d <- positive
  let targets = map MkPattern $ take len [5,105..]
  return $ buildSpecialSSOMandTargets ps r0 d targets

instance Arbitrary SpecialSSOMandTargets where
  arbitrary = sized sizedSpecialSSOMandTargets

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern (which would render the test invalid).
prop_batch_training_works2 :: SpecialSSOMandTargets -> Property
prop_batch_training_works2 (SpecialSSOMandTargets s xs) =
  errBefore /= 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          errBefore = absDiff (sort xs) (sort (models s))
          errAfter = absDiff (sort xs) (sort (models s'))

data IncompleteSSOMandTargets = IncompleteSSOMandTargets (SSOM
  (Exponential Double) Int Int TestPattern) [TestPattern] deriving Show

buildIncompleteSSOMandTargets
  :: [TestPattern] -> Double -> Double -> [TestPattern]
    -> IncompleteSSOMandTargets
buildIncompleteSSOMandTargets ps r0 d targets =
  IncompleteSSOMandTargets s targets
    where gm = M.fromList . zip [0..] $ ps
          s = SSOM gm (Exponential r0 d) 0

-- | Same as sizedSSOMandTargets, except some nodes don't have a value.
sizedIncompleteSSOMandTargets :: Int -> Gen IncompleteSSOMandTargets
sizedIncompleteSSOMandTargets n = do
  let len = n + 1
  ps <- vectorOf len arbitrary
  r0 <- choose (0, 1)
  d <- positive
  targets <- vectorOf len arbitrary
  return $ buildIncompleteSSOMandTargets ps r0 d targets

instance Arbitrary IncompleteSSOMandTargets where
  arbitrary = sized sizedIncompleteSSOMandTargets

prop_can_train_incomplete_SSOM :: IncompleteSSOMandTargets -> Property
prop_can_train_incomplete_SSOM (IncompleteSSOMandTargets s xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ toDouble x - toDouble (toMap s M.! bmu)
          errAfter = abs $ toDouble x - toDouble (toMap s' M.! bmu)

test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SSOM"
  [
    testProperty "prop_Exponential_starts_at_r0"
      prop_Exponential_starts_at_r0,
    testProperty "prop_Exponential_ge_0"
      prop_Exponential_ge_0,
    testProperty "prop_training_reduces_error"
      prop_training_reduces_error,
    testProperty "prop_classifyAndTrainEquiv"
      prop_classifyAndTrainEquiv,
    testProperty "prop_diffAndTrainEquiv" prop_diffAndTrainEquiv,
    testProperty "prop_trainNodeEquiv" prop_trainNodeEquiv,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent,
    testProperty "prop_batch_training_works2"
      prop_batch_training_works2,
    testProperty "prop_can_train_incomplete_SSOM"
      prop_can_train_incomplete_SSOM
  ]
