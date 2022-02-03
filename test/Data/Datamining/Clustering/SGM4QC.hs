------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGM4QC
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}

module Data.Datamining.Clustering.SGM4QC
  (
    test
  ) where

import           Control.DeepSeq                         (NFData, deepseq)
import           Data.Datamining.Clustering.SGM4Internal
import           Data.Datamining.Pattern.Numeric         (absDifference,
                                                          makeOrdFractionalSimilar)
import           Data.List                               (minimumBy)
import qualified Data.Map.Strict                         as M
import           Data.Ord                                (comparing)
import           Data.Word                               (Word16)
import           GHC.Generics                            (Generic)
import           System.Random                           (Random)
import           Test.Framework                          as TF (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck                         (Arbitrary, Gen,
                                                          Positive, Property,
                                                          arbitrary, choose,
                                                          getPositive, shrink,
                                                          sized, suchThat,
                                                          vectorOf, (==>))

newtype UnitInterval a = UnitInterval {getUnitInterval :: a}
 deriving ( Eq, Ord, Show, Read)

instance Functor UnitInterval where
  fmap f (UnitInterval x) = UnitInterval (f x)

instance (Num a, Ord a, Random a, Arbitrary a)
    => Arbitrary (UnitInterval a) where
  arbitrary = UnitInterval <$> choose (0,1)
  shrink (UnitInterval x) =
    [ UnitInterval x' | x' <- shrink x, x' >= 0, x' <= 1]

prop_Exponential_starts_at_r0
  :: UnitInterval Double -> Positive Double -> Bool
prop_Exponential_starts_at_r0 r0 d
  = abs (exponential r0' d' 0 - r0') < 0.01
  where r0' = getUnitInterval r0
        d' = getPositive d

prop_Exponential_ge_0
  :: UnitInterval Double -> Positive Double -> Positive Int -> Bool
prop_Exponential_ge_0 r0 d t = exponential r0' d' t' >= 0
  where r0' = getUnitInterval r0
        d' = getPositive d
        t' = getPositive t

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

data TestAdjuster = TestAdjuster Double Double
  deriving (Eq, Read, Show, Generic, NFData)

instance Adjuster TestAdjuster where
  type TimeType TestAdjuster = Int
  type MetricType TestAdjuster = Double
  type PatternType TestAdjuster = Double
  learningRate (TestAdjuster r0 d) = exponential r0 d
  difference _ = absDifference
  makeSimilar _ = makeOrdFractionalSimilar

data TestSGM = TestSGM (SGM TestAdjuster Int Word16 Double)
  deriving (Eq, Read, Show)

buildTestSGM
  :: Double -> Double -> Int -> [Double] -> [(Word16, Double)] -> TestSGM
buildTestSGM r0 d maxSz ps kps = TestSGM s''
  where adj = TestAdjuster r0 d
        s = makeSGM adj maxSz
        s' = trainBatch s ps
        s'' = imprintBatch s' kps

sizedTestSGM :: Int -> Gen TestSGM
sizedTestSGM n = do
  maxSz <- choose (1, min (n+1) 1023)
  numTrainingPatterns <- choose (0, n)
  let numImprintPatterns = n - numTrainingPatterns
  r0 <- choose (0, 1)
  d <- positive
  ps <- vectorOf numTrainingPatterns arbitrary
  kps <- vectorOf numImprintPatterns arbitrary
  return $ buildTestSGM r0 d maxSz ps kps

instance Arbitrary TestSGM where
  arbitrary = sized sizedTestSGM

prop_classify_chooses_best_fit :: TestSGM -> Double -> Property
prop_classify_chooses_best_fit (TestSGM s) x
  = not (isEmpty s) ==> bmu == bmu2
  where (bmu, _, report) = classify s x
        bmu2 = fst (minimumBy (comparing f) . M.toList $ report)
        f (_, (_, d)) = d

prop_trainAndClassify_chooses_best_fit :: TestSGM -> Double -> Bool
prop_trainAndClassify_chooses_best_fit (TestSGM s) x = bmu == bmu2
  where (bmu, _, report, _) = trainAndClassify s x
        bmu2 = fst (minimumBy (comparing f) . M.toList $ report)
        f (_, (_, d)) = d

prop_classify_never_creates_model :: TestSGM -> Double -> Property
prop_classify_never_creates_model (TestSGM s) x
  = not (isEmpty s) ==> bmu `elem` labels s
  where (bmu, _, _) = classify s x

prop_classify_never_causes_error_unless_som_empty
  :: TestSGM -> Double -> Property
prop_classify_never_causes_error_unless_som_empty (TestSGM s) p
  = not (isEmpty s) ==> deepseq (classify s p) True

prop_trainNode_reduces_diff :: TestSGM -> Double -> Property
prop_trainNode_reduces_diff (TestSGM s) x = not (isEmpty s) ==>
  diffAfter < diffBefore || diffBefore == 0 || lr < 1e-10
  where (bmu, diffBefore, _) = classify s x
        s2 = trainNode s bmu x
        (_, diffAfter, _) = classify s2 x
        lr = lrf . fromIntegral $ time s
        lrf = learningRate (adjuster s)

prop_training_reduces_diff :: TestSGM -> Double -> Property
prop_training_reduces_diff (TestSGM s) x = not (isEmpty s) ==>
  diffAfter < diffBefore || diffBefore == 0 || lr < 1e-10
  where (_, diffBefore, _) = classify s x
        s2 = train s x
        (_, diffAfter, _) = classify s2 x
        lr = lrf . fromIntegral $ time s
        lrf = learningRate (adjuster s)

-- TODO prop: map will never exceed capacity

prop_addNode_never_causes_error :: TestSGM -> Double -> Property
prop_addNode_never_causes_error (TestSGM s) p
  = size s < capacity s ==> deepseq (addNode s p) True

prop_train_never_causes_error :: TestSGM -> Double -> Bool
prop_train_never_causes_error (TestSGM s) p
  = deepseq (train s p) True

prop_train_only_modifies_one_model
  :: TestSGM -> Double -> Property
prop_train_only_modifies_one_model (TestSGM s) p
  = size s < capacity s ==> otherModelsBefore == otherModelsAfter
    where (bmu, _, _, s2) = trainAndClassify s p
          otherModelsBefore = M.delete bmu . M.map fst . toMap $ s
          otherModelsAfter = M.delete bmu . M.map fst . toMap $ s2

prop_train_increments_counter :: TestSGM -> Double -> Property
prop_train_increments_counter (TestSGM s) x
  = size s < capacity s ==> countAfter == countBefore + 1
  -- We have to check if the SGM is full, otherwise we'll replace an
  -- existing model (and its counter), which means that the total
  -- count could change by an arbitrary amount.
  where countBefore = time s
        countAfter = time $ train s x

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: TestSGM -> [Double] -> Bool
prop_batch_training_works (TestSGM s) ps
  -- = capacity s > length ps
  --   ==> classifications == (concat . replicate 5) firstSet
  = classifications == (concat . replicate n) firstSet
  where trainingSet = (concat . replicate n) ps
        n = 4
        sRightSize = if capacity s >= length ps
          then s
          else s { capacity=length ps + 1}
        s' = trainBatch sRightSize trainingSet
        classifications = map (justBMU . classify s') trainingSet
        justBMU = \(bmu, _, _) -> bmu
        firstSet = take (length ps) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent :: TestSGM -> Double -> Bool
prop_classification_is_consistent (TestSGM s) x = bmu == bmu'
  where (bmu, _, _, s2) = trainAndClassify s x
        (bmu', _, _) = classify s2 x

prop_classification_stabilises :: TestSGM -> [Double] -> Property
prop_classification_stabilises (TestSGM s)  ps
  = (not . null $ ps) && capacity s > 1 + length ps ==> k2 == k1
  where sStable = trainBatch s . concat . replicate 8 $ ps
        (k1, _, _, sStable2) = trainAndClassify sStable (head ps)
        sStable3 = trainBatch sStable2 ps
        (k2, _, _) = classify sStable3 (head ps)

prop_imprint_never_causes_error :: TestSGM -> Word16 -> Double -> Bool
prop_imprint_never_causes_error (TestSGM s) k p
  = deepseq (imprint s k p) True


test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SGM4"
  [
    testProperty "prop_Exponential_starts_at_r0"
      prop_Exponential_starts_at_r0,
    testProperty "prop_Exponential_ge_0"
      prop_Exponential_ge_0,
    testProperty "prop_addNode_never_causes_error"
      prop_addNode_never_causes_error,
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
      prop_classification_stabilises,
    testProperty "prop_imprint_never_causes_error"
      prop_imprint_never_causes_error
  ]
