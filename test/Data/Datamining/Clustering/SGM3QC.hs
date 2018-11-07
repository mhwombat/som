------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGM3QC
-- Copyright   :  (c) Amy de Buitléir 2012-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}

module Data.Datamining.Clustering.SGM3QC
  (
    test
  ) where

import           Control.DeepSeq
    (deepseq)
import           Data.Datamining.Clustering.SGM2Internal
import           Data.Datamining.Pattern
    (absDifference, adjustNum)
import           Data.List
    (minimumBy)
import qualified Data.Map.Strict                         as M
import           Data.Ord
    (comparing)
import           Data.Word
    (Word16)
import           System.Random
    (Random)
import           Test.Framework                          as TF
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck
    ( Arbitrary
    , Gen
    , Positive
    , Property
    , arbitrary
    , choose
    , getPositive
    , property
    , shrink
    , sized
    , suchThat
    , vectorOf
    , (==>)
    )

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

data TestSGM = TestSGM (SGM Int Double Word16 Double) String

instance Show TestSGM where
  show (TestSGM _ desc) = desc

buildTestSGM
  :: Double -> Double -> Int -> [Double] -> TestSGM
buildTestSGM r0 d maxSz ps = TestSGM s' desc
  where lrf = exponential r0 d
        s = makeSGM lrf maxSz absDifference adjustNum
        desc = "buildTestSGM " ++ show r0 ++ " " ++ show d
                 ++ " " ++ show maxSz
                 ++ " " ++ show ps
        s' = trainBatch s ps

sizedTestSGM :: Int -> Gen TestSGM
sizedTestSGM n = do
  maxSz <- choose (1, min (n+1) 1023)
  let numPatterns = n
  r0 <- choose (0, 1)
  d <- positive
  ps <- vectorOf numPatterns arbitrary
  return $ buildTestSGM r0 d maxSz ps

instance Arbitrary TestSGM where
  arbitrary = sized sizedTestSGM

prop_classify_chooses_best_fit :: TestSGM -> Double -> Property
prop_classify_chooses_best_fit (TestSGM s _) x
  = not (isEmpty s) ==> property $ bmu == bmu2
  where (bmu, _, report) = classify s x
        bmu2 = fst (minimumBy (comparing f) . M.toList $ report)
        f (_, (_, d)) = d

prop_trainAndClassify_chooses_best_fit :: TestSGM -> Double -> Property
prop_trainAndClassify_chooses_best_fit (TestSGM s _) x
  = property $ bmu == bmu2
  where (bmu, _, report, _) = trainAndClassify s x
        bmu2 = fst (minimumBy (comparing f) . M.toList $ report)
        f (_, (_, d)) = d

prop_classify_never_creates_model :: TestSGM -> Double -> Property
prop_classify_never_creates_model (TestSGM s _) x
  = not (isEmpty s) ==> bmu `elem` (labels s)
  where (bmu, _, _) = classify s x

prop_classify_never_causes_error_unless_som_empty
  :: TestSGM -> Double -> Property
prop_classify_never_causes_error_unless_som_empty (TestSGM s _) p
  = not (isEmpty s) ==> property $ deepseq (classify s p) True

prop_trainNode_reduces_diff :: TestSGM -> Double -> Property
prop_trainNode_reduces_diff (TestSGM s _) x = not (isEmpty s) ==>
  diffAfter < diffBefore || diffBefore == 0
                         || learningRate s (time s) < 1e-10
  where (bmu, diffBefore, _) = classify s x
        s2 = trainNode s bmu x
        (_, diffAfter, _) = classify s2 x

prop_training_reduces_diff :: TestSGM -> Double -> Property
prop_training_reduces_diff (TestSGM s _) x = not (isEmpty s) ==>
  diffAfter < diffBefore || diffBefore == 0
                         || learningRate s (time s) < 1e-10
  where (_, diffBefore, _) = classify s x
        s2 = train s x
        (_, diffAfter, _) = classify s2 x

-- TODO prop: map will never exceed capacity

prop_train_never_causes_error :: TestSGM -> Double -> Property
prop_train_never_causes_error (TestSGM s _) p
  = property $ deepseq (train s p) True

prop_train_only_modifies_one_model
  :: TestSGM -> Double -> Property
prop_train_only_modifies_one_model (TestSGM s _) p
  = size s < capacity s ==> otherModelsBefore == otherModelsAfter
    where (bmu, _, _, s2) = trainAndClassify s p
          otherModelsBefore = M.delete bmu . M.map fst . toMap $ s
          otherModelsAfter = M.delete bmu . M.map fst . toMap $ s2

prop_train_increments_counter :: TestSGM -> Double -> Property
prop_train_increments_counter (TestSGM s _) x
  = size s < capacity s ==> countAfter == countBefore + 1
  -- We have to check if the SGM is full, otherwise we'll replace an
  -- existing model (and its counter), which means that the total
  -- count could change by an arbitrary amount.
  where countBefore = time s
        countAfter = time $ train s x

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: TestSGM -> [Double] -> Property
prop_batch_training_works (TestSGM s _) ps
  -- = capacity s > length ps
  --   ==> classifications == (concat . replicate 5) firstSet
  = property $ classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) ps
        sRightSize = if capacity s >= length ps
          then s
          else s { capacity=length ps + 1}
        s' = trainBatch sRightSize trainingSet
        classifications = map (justBMU . classify s') trainingSet
        justBMU = \(bmu, _, _) -> bmu
        firstSet = take (length ps) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent :: TestSGM -> Double -> Property
prop_classification_is_consistent (TestSGM s _) x
  = property $ bmu == bmu'
  where (bmu, _, _, s2) = trainAndClassify s x
        (bmu', _, _) = classify s2 x

prop_classification_stabilises :: TestSGM -> [Double] -> Property
prop_classification_stabilises (TestSGM s _)  ps
  = (not . null $ ps) && capacity s > length ps ==> k2 == k1
  where sStable = trainBatch s . concat . replicate 10 $ ps
        (k1, _, _, sStable2) = trainAndClassify sStable (head ps)
        sStable3 = trainBatch sStable2 ps
        (k2, _, _) = classify sStable3 (head ps)


test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SGM2"
  [
    testProperty "prop_Exponential_starts_at_r0"
      prop_Exponential_starts_at_r0,
    testProperty "prop_Exponential_ge_0"
      prop_Exponential_ge_0,
    testProperty "prop_classify_chooses_best_fit"
      prop_classify_chooses_best_fit,
    testProperty "prop_trainAndClassify_chooses_best_fit"
      prop_trainAndClassify_chooses_best_fit,
    testProperty "prop_classify_never_creates_model"
      prop_classify_never_creates_model,
    testProperty "prop_classify_never_causes_error_unless_som_empty"
      prop_classify_never_causes_error_unless_som_empty,
    testProperty "prop_trainNode_reduces_diff"
      prop_trainNode_reduces_diff,
    testProperty "prop_training_reduces_diff"
      prop_training_reduces_diff,
    testProperty "prop_train_never_causes_error"
      prop_train_never_causes_error,
    testProperty "prop_train_only_modifies_one_model"
      prop_train_only_modifies_one_model,
    testProperty "prop_train_increments_counter"
      prop_train_increments_counter,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent,
    testProperty "prop_classification_stabilises"
      prop_classification_stabilises
  ]
