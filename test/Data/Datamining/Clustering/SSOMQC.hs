------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SSOMQC
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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

import Data.Datamining.Pattern (euclideanDistanceSquared, adjustNum,
  absDifference)
import Data.Datamining.Clustering.Classifier(classify,
  classifyAndTrain, reportAndTrain, differences, diffAndTrain, models,
  train, trainBatch, numModels)
import Data.Datamining.Clustering.SSOMInternal
import qualified Data.Map.Strict as M

import Data.List (sort)
import System.Random (Random)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Gen, Arbitrary, Property, Positive,
  arbitrary, shrink, choose, property, sized, suchThat, vectorOf,
  getPositive)

newtype UnitInterval a = UnitInterval {getUnitInterval :: a}
 deriving ( Eq, Ord, Show, Read)

instance Functor UnitInterval where
  fmap f (UnitInterval x) = UnitInterval (f x)

instance (Num a, Ord a, Random a, Arbitrary a)
    => Arbitrary (UnitInterval a) where
  arbitrary = fmap UnitInterval $ choose (0,1)
  shrink (UnitInterval x) =
    [ UnitInterval x' | x' <- shrink x, x' >= 0, x' <= 1]

prop_Exponential_starts_at_r0
  :: UnitInterval Double -> Positive Double -> Property
prop_Exponential_starts_at_r0 r0 d
  = property $ abs (exponential r0' d' 0 - r0') < 0.01
  where r0' = getUnitInterval r0
        d' = getPositive d

prop_Exponential_ge_0
  :: UnitInterval Double -> Positive Double -> Positive Double -> Property
prop_Exponential_ge_0 r0 d t = property $ exponential r0' d' t' >= 0
  where r0' = getUnitInterval r0
        d' = getPositive d
        t' = getPositive t

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

-- | A classifier and a training set. The training set will consist of
--   @j@ vectors of equal length, where @j@ is the number of patterns
--   the classifier can model. After running through the training set a
--   few times, the classifier should be very accurate at identifying
--   any of those @j@ vectors.
data SSOMTestData
  = SSOMTestData
    {
      som1 :: SSOM Double Double Int Double,
      learningRateDesc1 :: String,
      trainingSet1 :: [Double]
    }

instance Show SSOMTestData where
  show s = "buildSSOMTestData " ++ show (M.elems . sMap . som1 $ s)
    ++ " " ++ learningRateDesc1 s 
    ++ " " ++ show (trainingSet1 s) 

buildSSOMTestData
  :: [Double] -> Double -> Double -> [Double] -> SSOMTestData
buildSSOMTestData ps r0 d targets =
  SSOMTestData s desc targets
    where gm = M.fromList . zip [0..] $ ps
          lrf = exponential r0 d
          s = SSOM gm lrf absDifference adjustNum 0
          desc = show r0 ++ " " ++ show d

sizedSSOMTestData :: Int -> Gen SSOMTestData
sizedSSOMTestData n = do
  let len = n + 1
  ps <- vectorOf len arbitrary
  r0 <- choose (0, 1)
  d <- positive
  targets <- vectorOf len arbitrary
  return $ buildSSOMTestData ps r0 d targets

instance Arbitrary SSOMTestData where
  arbitrary = sized sizedSSOMTestData

prop_training_reduces_error :: SSOMTestData -> Property
prop_training_reduces_error (SSOMTestData s _ xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ x - (toMap s M.! bmu)
          errAfter = abs $ x - (toMap s' M.! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: SSOMTestData -> Property
prop_classifyAndTrainEquiv (SSOMTestData s _ ps) = property $
  bmu == s `classify` p && toMap s1 == toMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: SSOMTestData -> Property
prop_diffAndTrainEquiv (SSOMTestData s _ ps) = property $
  diffs == s `differences` p && toMap s1 == toMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNode s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNodeEquiv :: SSOMTestData -> Property
prop_trainNodeEquiv (SSOMTestData s _ ps) = property $
  toMap s1 == toMap s2
    where p = head ps
          s1 = trainNode s (classify s p) p
          s2 = train s p

prop_train_node_only_modifies_one_model :: Int -> SSOMTestData -> Property
prop_train_node_only_modifies_one_model n (SSOMTestData s _ ps)
  = property $ as == as' && bs == bs'
    where p = head ps
          k = n `mod` (numModels s)
          s' = trainNode s k p
          (as, _:bs) = splitAt k (models s)
          (as', _:bs') = splitAt k (models s')

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: SSOMTestData -> Property
prop_batch_training_works (SSOMTestData s _ xs) = property $
  classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent :: SSOMTestData -> Property
prop_classification_is_consistent (SSOMTestData s _ (x:_))
  = property $ bmu == bmu'
  where (bmu, _, s') = reportAndTrain s x
        (bmu', _, _) = reportAndTrain s' x
prop_classification_is_consistent _ = error "Should not happen"

-- | Same as SSOMTestData, except that the initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern.
data SpecialSSOMTestData
  = SpecialSSOMTestData
    {
      som2 :: SSOM Double Double Int Double,
      learningRateDesc2 :: String,
      trainingSet2 :: [Double]
    }

instance Show SpecialSSOMTestData where
  show s = "buildSpecialSSOMTestData "
    ++ show (M.elems . sMap . som2 $ s)
    ++ " " ++ learningRateDesc2 s 
    ++ " " ++ show (trainingSet2 s) 

buildSpecialSSOMTestData
  :: [Double] -> Double -> Double -> [Double] -> SpecialSSOMTestData
buildSpecialSSOMTestData ps r0 d targets =
  SpecialSSOMTestData s desc targets
    where gm = M.fromList . zip [0..] $ ps
          lrf = exponential r0 d
          s = SSOM gm lrf absDifference adjustNum 0
          desc = show r0 ++ " " ++ show d

sizedSpecialSSOMTestData :: Int -> Gen SpecialSSOMTestData
sizedSpecialSSOMTestData n = do
  let len = n + 1
  let ps = take len [0,100..]
  r0 <- choose (0, 1)
  d <- positive
  let targets = take len [5,105..]
  return $ buildSpecialSSOMTestData ps r0 d targets

instance Arbitrary SpecialSSOMTestData where
  arbitrary = sized sizedSpecialSSOMTestData

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern (which would render the test invalid).
prop_batch_training_works2 :: SpecialSSOMTestData -> Property
prop_batch_training_works2 (SpecialSSOMTestData s _ xs) =
  errBefore /= 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          errBefore = euclideanDistanceSquared (sort xs) (sort (models s))
          errAfter = euclideanDistanceSquared (sort xs) (sort (models s'))

-- | Same as sizedSSOMTestData, except some nodes don't have a value.
data IncompleteSSOMTestData
  = IncompleteSSOMTestData
    {
      som3 :: SSOM Double Double Int Double,
      learningRateDesc3 :: String,
      trainingSet3 :: [Double]
    }

instance Show IncompleteSSOMTestData where
  show s = "buildIncompleteSSOMTestData "
    ++ show (M.elems . sMap . som3 $ s)
    ++ " " ++ learningRateDesc3 s 
    ++ " " ++ show (trainingSet3 s) 

buildIncompleteSSOMTestData
  :: [Double] -> Double -> Double -> [Double] -> IncompleteSSOMTestData
buildIncompleteSSOMTestData ps r0 d targets =
  IncompleteSSOMTestData s desc targets
    where gm = M.fromList . zip [0..] $ ps
          lrf = exponential r0 d
          s = SSOM gm lrf absDifference adjustNum 0
          desc = show r0 ++ " " ++ show d

sizedIncompleteSSOMTestData :: Int -> Gen IncompleteSSOMTestData
sizedIncompleteSSOMTestData n = do
  let len = n + 1
  ps <- vectorOf len arbitrary
  r0 <- choose (0, 1)
  d <- positive
  targets <- vectorOf len arbitrary
  return $ buildIncompleteSSOMTestData ps r0 d targets

instance Arbitrary IncompleteSSOMTestData where
  arbitrary = sized sizedIncompleteSSOMTestData

prop_can_train_incomplete_SSOM :: IncompleteSSOMTestData -> Property
prop_can_train_incomplete_SSOM (IncompleteSSOMTestData s _ xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ x - (toMap s M.! bmu)
          errAfter = abs $ x - (toMap s' M.! bmu)

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
    testProperty "prop_train_node_only_modifies_one_model"
      prop_train_node_only_modifies_one_model,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent,
    testProperty "prop_batch_training_works2"
      prop_batch_training_works2,
    testProperty "prop_can_train_incomplete_SSOM"
      prop_can_train_incomplete_SSOM
  ]
