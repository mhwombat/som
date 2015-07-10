------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOSQC
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

module Data.Datamining.Clustering.SOSQC
  (
    test
  ) where

import Prelude hiding (null)

import Data.Datamining.Pattern (adjustNum, absDifference)
import Data.Datamining.Clustering.Classifier(classify,
  classifyAndTrain, reportAndTrain, differences, diffAndTrain, models,
  train, trainBatch, numModels)
import Data.Datamining.Clustering.SOSInternal
import qualified Data.Map.Strict as M

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
  :: UnitInterval Double -> Positive Double -> Positive Int -> Property
prop_Exponential_ge_0 r0 d t = property $ exponential r0' d' t' >= 0
  where r0' = getUnitInterval r0
        d' = getPositive d
        t' = getPositive t

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

data UntrainedTestSOS
  = UntrainedTestSOS
    {
      som0 :: SOS Int Double Int Double,
      learningRateDesc0 :: String
    }

instance Show UntrainedTestSOS where
  show s = "buildUntrainedTestSOS " ++ learningRateDesc0 s 
    ++ " " ++ show (maxSize (som0 s))
    ++ " " ++ show (diffThreshold (som0 s))

buildUntrainedTestSOS
  :: Double -> Double -> Int -> Double -> UntrainedTestSOS
buildUntrainedTestSOS r0 d maxSz dt = UntrainedTestSOS s desc
  where lrf = exponential r0 d
        s = makeSOS lrf maxSz dt absDifference adjustNum :: SOS Int Double Int Double
        desc = show r0 ++ " " ++ show d
  
sizedUntrainedTestSOS :: Int -> Gen UntrainedTestSOS
sizedUntrainedTestSOS n = do
  let maxSz = n + 1
  r0 <- choose (0, 1)
  d <- positive
  dt <- choose (0, 1)
  return $ buildUntrainedTestSOS r0 d maxSz dt
  
instance Arbitrary UntrainedTestSOS where
  arbitrary = sized sizedUntrainedTestSOS

-- | A classifier and a training set. The training set will consist of
--   @j@ vectors of equal length, where @j@ is the number of patterns
--   the classifier can model. After running through the training set a
--   few times, the classifier should be very accurate at identifying
--   any of those @j@ vectors.
data SOSTestData
  = SOSTestData
    {
      som1 :: SOS Int Double Int Double,
      learningRateDesc1 :: String,
      trainingSet1 :: [Double]
    }

instance Show SOSTestData where
  show s = "buildSOSTestData " ++ show (models . som1 $ s)
    ++ " " ++ show (M.elems . counters . som1 $ s) 
    ++ " " ++ learningRateDesc1 s 
    ++ " " ++ show (maxSize (som1 s))
    ++ " " ++ show (diffThreshold (som1 s))
    ++ " " ++ show (trainingSet1 s) 

buildSOSTestData
  :: [Double] -> [Positive Int] -> Double -> Double -> Int -> Double -> [Double]
    -> SOSTestData
buildSOSTestData ps ks r0 d maxSz dt targets =
  SOSTestData s desc targets
    where gm = M.fromList . zip [0..] . zip ps $ map getPositive ks
          lrf = exponential r0 d
          s = SOS gm lrf maxSz dt absDifference adjustNum 0
          desc = show r0 ++ " " ++ show d

sizedSOSTestData :: Int -> Gen SOSTestData
sizedSOSTestData n = do
  maxSz <- choose (1, n+1)
  n1 <- choose (0, maxSz)
  ps <- vectorOf n1 arbitrary
  ks <- vectorOf n1 arbitrary
  r0 <- choose (0, 1)
  d <- positive
  dt <- choose (0, 1)
  n2 <- choose (1, n+1)
  targets <- vectorOf n2 arbitrary
  return $ buildSOSTestData ps ks r0 d maxSz dt targets

instance Arbitrary SOSTestData where
  arbitrary = sized sizedSOSTestData

prop_trainNode_reduces_diff :: SOSTestData -> Property
prop_trainNode_reduces_diff (SOSTestData s _ xs) = not (null s) ==>
  diffAfter < diffBefore || diffBefore == 0
                         || learningRate s (time s) < 1e-10
  where x = head xs
        (bmu, diffBefore) = findBMU s x
        s' = trainNode s bmu x
        (_, diffAfter) = findBMU s' x

prop_diff_lt_threshold_after_training :: SOSTestData -> Property
prop_diff_lt_threshold_after_training (SOSTestData s _ xs) =
  property $ diffAfter < diffThreshold s
  where s' = justTrain s x
        x = head xs
        (_, diffAfter) = findBMU s' x

prop_training_reduces_diff :: SOSTestData -> Property
prop_training_reduces_diff (SOSTestData s _ xs) = not (null s) ==>
  diffAfter < diffBefore || diffBefore == 0
                         || learningRate s (time s) < 1e-10
  where x = head xs
        (_, diffBefore) = findBMU s x
        (_, s') = classifyAndTrain s x
        (_, diffAfter) = findBMU s' x

-- TODO prop: map will never exceed maxSize

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: SOSTestData -> Property
prop_classifyAndTrainEquiv (SOSTestData s _ ps) = not (null s) ==>
  bmu == s `classify` p && toMap s1 == toMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: SOSTestData -> Property
prop_diffAndTrainEquiv (SOSTestData s _ ps) = not (null s) ==>
  diffs == s `differences` p && toMap s1 == toMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNode s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNodeEquiv :: SOSTestData -> Property
prop_trainNodeEquiv (SOSTestData s _ ps) = not (null s) ==>
  toMap s1 == toMap s2
    where p = head ps
          s1 = trainNode s (classify s p) p
          s2 = train s p

prop_train_node_only_modifies_one_model :: Int -> SOSTestData -> Property
prop_train_node_only_modifies_one_model n (SOSTestData s _ ps)
  = not (null s) ==> as == as' && bs == bs'
    where p = head ps
          k = n `mod` (numModels s)
          s' = trainNode s k p
          (as, _:bs) = splitAt k (models s)
          (as', _:bs') = splitAt k (models s')

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: SOSTestData -> Property
prop_batch_training_works (SOSTestData s _ xs) = not (null s) ==>
  classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent :: SOSTestData -> Property
prop_classification_is_consistent (SOSTestData s _ (x:_))
  = not (null s) ==> bmu == bmu'
  where (bmu, _, s') = reportAndTrain s x
        (bmu', _, _) = reportAndTrain s' x
prop_classification_is_consistent _ = error "Should not happen"

-- -- | Same as SOSTestData, except that the training set is designed to
-- --   ensure that a single node will NOT train to more than one pattern.
-- data SpecialSOSTestData = Special SOSTestData deriving Show

-- sizedSpecialSOSTestData :: Int -> Gen SpecialSOSTestData
-- sizedSpecialSOSTestData n = do
--   x <- sizedSOSTestData n
--   let len = length (trainingSet1 x)
--   let ps = take len [0,100..]
--   return . Special $ x { trainingSet1 = ps }

-- instance Arbitrary SpecialSOSTestData where
--   arbitrary = sized sizedSpecialSOSTestData

-- -- | If we train a classifier once on a set of patterns, where the
-- --   number of patterns in the set is equal to the number of nodes in
-- --   the classifier, then the classifier should become a better
-- --   representation of the training set. The initial models and training
-- --   set are designed to ensure that a single node will NOT train to
-- --   more than one pattern (which would render the test invalid).
-- prop_batch_training_works2 :: SpecialSOSTestData -> Property
-- prop_batch_training_works2 (Special (SOSTestData s _ xs)) =
--   errBefore /= 0 ==> errAfter < errBefore
--     where s' = trainBatch s xs
--           modelsAfter = models s'
--           modelsBefore = take (length modelsAfter) $ models s ++ repeat 0
--           errBefore = euclideanDistanceSquared (sort xs) (sort modelsBefore)
--           errAfter = euclideanDistanceSquared (sort xs) (sort modelsAfter)

test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SOS"
  [
    testProperty "prop_Exponential_starts_at_r0"
      prop_Exponential_starts_at_r0,
    testProperty "prop_Exponential_ge_0"
      prop_Exponential_ge_0,
    testProperty "prop_trainNode_reduces_diff"
      prop_trainNode_reduces_diff,
    testProperty "prop_diff_lt_threshold_after_training"
      prop_diff_lt_threshold_after_training,
    testProperty "prop_training_reduces_diff"
      prop_training_reduces_diff,
    testProperty "prop_classifyAndTrainEquiv"
      prop_classifyAndTrainEquiv,
    testProperty "prop_diffAndTrainEquiv" prop_diffAndTrainEquiv,
    testProperty "prop_trainNodeEquiv" prop_trainNodeEquiv,
    testProperty "prop_train_node_only_modifies_one_model"
      prop_train_node_only_modifies_one_model,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent
  ]
