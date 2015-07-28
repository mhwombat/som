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

import Data.Datamining.Pattern (adjustNum, absDifference)
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

data TestSOS = TestSOS (SOS Int Double Int Double) String

instance Show TestSOS where
  show (TestSOS _ desc) = desc

buildTestSOS
  :: Double -> Double -> Int -> Double -> [Double] -> TestSOS
buildTestSOS r0 d maxSz dt ps = TestSOS s' desc
  where lrf = exponential r0 d
        s = makeSOS lrf maxSz dt absDifference adjustNum :: SOS Int Double Int Double
        desc = "buildTestSOS " ++ show r0 ++ " " ++ show d
                 ++ " " ++ show maxSz
                 ++ " " ++ show dt
                 ++ " " ++ show ps
        s' = trainBatch s ps

sizedTestSOS :: Int -> Gen TestSOS
sizedTestSOS n = do
  maxSz <- choose (1, n+1)
  let numPatterns = n
  r0 <- choose (0, 1)
  d <- positive
  dt <- choose (0, 1)
  ps <- vectorOf numPatterns arbitrary
  return $ buildTestSOS r0 d maxSz dt ps

instance Arbitrary TestSOS where
  arbitrary = sized sizedTestSOS

prop_trainNode_reduces_diff :: TestSOS -> Double -> Property
prop_trainNode_reduces_diff (TestSOS s _) x = not (isEmpty s) ==>
  diffAfter < diffBefore || diffBefore == 0
                         || learningRate s (time s) < 1e-10
  where (bmu, diffBefore, _, s2) = classify s x
        s3 = trainNode s2 bmu x
        (_, diffAfter, _, _) = classify s3 x

prop_diff_lt_threshold_after_training :: TestSOS -> Double -> Property
prop_diff_lt_threshold_after_training (TestSOS s _) x =
  property $ diffAfter < diffThreshold s
  where s' = train s x
        (_, diffAfter, _, _) = classify s' x

prop_training_reduces_diff :: TestSOS -> Double -> Property
prop_training_reduces_diff (TestSOS s _) x = not (isEmpty s) ==>
  diffAfter < diffBefore || diffBefore == 0
                         || learningRate s (time s) < 1e-10
  where (_, diffBefore, _, s2) = classify s x
        s3 = train s2 x
        (_, diffAfter, _, _) = classify s3 x

-- TODO prop: map will never exceed maxSize

prop_train_only_modifies_one_model
  :: TestSOS -> Double -> Property
prop_train_only_modifies_one_model (TestSOS s _) p
  = numModels s < maxSize s ==> otherModelsBefore == otherModelsAfter
    where (bmu, _, _, s2) = classify s p
          s3 = train s2 p
          otherModelsBefore = M.delete bmu . M.map fst . toMap $ s2
          otherModelsAfter = M.delete bmu . M.map fst . toMap $ s3

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: TestSOS -> [Double] -> Property
prop_batch_training_works (TestSOS s _) ps
  = maxSize s > length ps
    ==> classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) ps
        s' = trainBatch s trainingSet
        classifications = map (justBMU . classify s') trainingSet
        justBMU = \(bmu, _, _, _) -> bmu
        firstSet = take (length ps) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent :: TestSOS -> Double -> Property
prop_classification_is_consistent (TestSOS s _) x
  = property $ bmu == bmu'
  where (bmu, _, _, s2) = classify s x
        s3 = train s2 x
        (bmu', _, _, _) = classify s3 x

prop_classification_stabilises
  :: TestSOS -> [Double] -> Property
prop_classification_stabilises (TestSOS s _)  ps
  = (not . null $ ps) && maxSize s > length ps ==> k2 == k1
  where sStable = trainBatch s . concat . replicate 10 $ ps
        (k1, _, _, sStable2) = classify sStable (head ps)
        sStable3 = trainBatch sStable2 ps
        (k2, _, _, _) = classify sStable3 (head ps)

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
    testProperty "prop_train_only_modifies_one_model"
      prop_train_only_modifies_one_model,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent,
    testProperty "prop_classification_stabilises"
      prop_classification_stabilises
  ]
