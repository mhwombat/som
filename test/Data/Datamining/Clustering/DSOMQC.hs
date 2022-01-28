------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.DSOMQC
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
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

module Data.Datamining.Clustering.DSOMQC
  (
    test
  ) where

import           Data.Datamining.Clustering.Classifier   (classify,
                                                          classifyAndTrain,
                                                          diffAndTrain,
                                                          differences, models,
                                                          numModels, train,
                                                          trainBatch)
import           Data.Datamining.Clustering.DSOMInternal
import           Data.Datamining.Pattern.List            (euclideanDistanceSquared,
                                                          magnitudeSquared)
import           Data.Datamining.Pattern.Numeric         (absDifference,
                                                          makeOrdFractionalSimilar)
import           Data.List                               (sort)
import           Math.Geometry.Grid                      (size)
import           Math.Geometry.Grid.Hexagonal            (HexHexGrid (..))
import           Math.Geometry.GridMap                   (elems, (!))
import           Math.Geometry.GridMap.Lazy              (LGridMap, lazyGridMap)
import           Test.Framework                          as TF (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck                         (Arbitrary, Gen,
                                                          Property, arbitrary,
                                                          choose, shrink, sized,
                                                          suchThat, vectorOf,
                                                          (==>))

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

data RougierArgs
  = RougierArgs Double Double Double Double Double deriving Show

instance Arbitrary RougierArgs where
  arbitrary = RougierArgs <$> choose (0,1) <*> choose (0,1)
                <*> arbitrary <*> choose (0,1) <*> positive

prop_rougierFunction_zero_if_perfect_model_exists :: RougierArgs -> Bool
prop_rougierFunction_zero_if_perfect_model_exists (RougierArgs r p _ diff dist) =
  rougierLearningFunction r p 0 diff dist == 0

prop_rougierFunction_r_if_bmu_is_bad_model :: RougierArgs -> Bool
prop_rougierFunction_r_if_bmu_is_bad_model (RougierArgs r p _ _ _) =
  rougierLearningFunction r p 1 1 0 == r

prop_rougierFunction_r_in_bounds :: RougierArgs -> Bool
prop_rougierFunction_r_in_bounds (RougierArgs r p bmuDiff diff dist)
  = 0 <= f && f <= 1
  where f = rougierLearningFunction r p bmuDiff diff dist

prop_rougierFunction_r_if_inelastic :: RougierArgs -> Bool
prop_rougierFunction_r_if_inelastic (RougierArgs r _ _ _ _) =
  rougierLearningFunction r 1.0 1.0 1.0 0 == r

fractionDiff :: [Double] -> [Double] -> Double
fractionDiff xs ys = if denom == 0 then 0 else d / denom
  where d = sqrt $ euclideanDistanceSquared xs ys
        denom = max xMag yMag
        xMag = sqrt $ magnitudeSquared xs
        yMag = sqrt $ magnitudeSquared ys

approxEqual :: [TestPattern] -> [TestPattern] -> Bool
approxEqual xs ys = fractionDiff xs' ys' <= 0.1
  where xs' = map toDouble xs
        ys' = map toDouble ys

-- We need to ensure that the absolute value of the difference
-- between any two test patterns is on the unit interval.

newtype TestPattern = TestPattern {toDouble :: Double}
 deriving ( Eq, Ord, Show, Read)

instance Arbitrary TestPattern where
  arbitrary = TestPattern <$> choose (0,1)
  shrink (TestPattern x) =
    [ TestPattern x' | x' <- shrink x, x' >= 0, x' <= 1]

testPatternDiff :: TestPattern -> TestPattern -> Double
testPatternDiff (TestPattern a) (TestPattern b) = absDifference a b

adjustTestPattern :: TestPattern -> Double -> TestPattern -> TestPattern
adjustTestPattern (TestPattern target) r (TestPattern x)
  = TestPattern $ makeOrdFractionalSimilar target r x

-- | A classifier and a training set. The training set will consist of
--   @j@ vectors of equal length, where @j@ is the number of patterns
--   the classifier can model. After running through the training set a
--   few times, the classifier should be very accurate at identifying
--   any of those @j@ vectors.
data DSOMTestData
  = DSOMTestData
    {
      som1         :: DSOM (LGridMap HexHexGrid) Double (Int, Int) TestPattern,
      params1      :: RougierArgs,
      trainingSet1 :: [TestPattern]
    }

instance Show DSOMTestData where
  show s = "buildDSOMTestData " ++ show (size . gridMap . som1 $ s)
    ++ " " ++ show (elems . gridMap . som1 $ s)
    ++ " (" ++ show (params1 s)
    ++ ") " ++ show (trainingSet1 s)

buildDSOMTestData
  :: Int -> [TestPattern] -> RougierArgs -> [TestPattern] -> DSOMTestData
buildDSOMTestData len ps rp@(RougierArgs r p _ _ _) =
  DSOMTestData s rp
    where g = HexHexGrid len
          gm = lazyGridMap g ps
          fr = rougierLearningFunction r p
          s = DSOM gm fr testPatternDiff adjustTestPattern

-- | Generate a classifier and a training set. The training set will
--   consist @j@ vectors of equal length, where @j@ is the number of
--   patterns the classifier can model. After running through the
--   training set a few times, the classifier should be very accurate at
--   identifying any of those @j@ vectors.
sizedDSOMTestData :: Int -> Gen DSOMTestData
sizedDSOMTestData n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let numberOfPatterns = tileCount
  ps <- vectorOf numberOfPatterns arbitrary
  rp <- arbitrary
  targets <- vectorOf numberOfPatterns arbitrary
  return $ buildDSOMTestData sideLength ps rp targets

instance Arbitrary DSOMTestData where
  arbitrary = sized sizedDSOMTestData

-- | If we use a fixed learning rate of one (regardless of the distance
--   from the BMU), and train a classifier once on one pattern, then all
--   nodes should match the input vector.
prop_global_instant_training_works :: DSOMTestData -> Bool
prop_global_instant_training_works (DSOMTestData s _ xs) =
  finalModels `approxEqual` expectedModels
    where x = head xs
          gm = toGridMap s :: LGridMap HexHexGrid TestPattern
          f _ _ _ = 1
          s2 = DSOM gm f testPatternDiff adjustTestPattern
          s3 = train s2 x
          finalModels = models s3 :: [TestPattern]
          expectedModels = replicate (numModels s) x :: [TestPattern]

prop_training_works :: DSOMTestData -> Property
prop_training_works (DSOMTestData s _ xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = testPatternDiff x (gridMap s ! bmu)
          errAfter = testPatternDiff x (gridMap s' ! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: DSOMTestData -> Bool
prop_classifyAndTrainEquiv (DSOMTestData s _ ps)
  = bmu == s `classify` p && gridMap s1 == gridMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: DSOMTestData -> Bool
prop_diffAndTrainEquiv (DSOMTestData s _ ps)
  = diffs == s `differences` p && gridMap s1 == gridMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNeighbourhood s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNeighbourhoodEquiv :: DSOMTestData -> Bool
prop_trainNeighbourhoodEquiv (DSOMTestData s _ ps)
  = gridMap s1 == gridMap s2
    where p = head ps
          s1 = trainNeighbourhood s (classify s p) p
          s2 = train s p

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: DSOMTestData -> Bool
prop_batch_training_works (DSOMTestData s _ xs)
  = classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

data SpecialDSOMTestData
  = SpecialDSOMTestData
    {
      som2         :: DSOM (LGridMap HexHexGrid) Double (Int, Int) TestPattern,
      params2      :: Double,
      trainingSet2 :: [TestPattern]
    }

instance Show SpecialDSOMTestData where
  show s = "buildDSOMTestData " ++ show (size . gridMap . som2 $ s)
    ++ " " ++ show (elems . gridMap . som2 $ s)
    ++ " (" ++ show (params2 s)
    ++ ") " ++ show (trainingSet2 s)

stepFunction :: Double -> Double -> Double -> Double -> Double
stepFunction r _ _ d = if d == 0 then r else 0.0

buildSpecialDSOMTestData
  :: Int -> [TestPattern] -> Double -> [TestPattern] -> SpecialDSOMTestData
buildSpecialDSOMTestData len ps r targets =
  SpecialDSOMTestData s r targets
    where g = HexHexGrid len
          gm = lazyGridMap g ps
          fr = stepFunction r
          s = DSOM gm fr testPatternDiff adjustTestPattern

-- | Generate a classifier and a training set. The training set will
--   consist @j@ vectors of equal length, where @j@ is the number of
--   patterns the classifier can model. After running through the
--   training set a few times, the classifier should be very accurate at
--   identifying any of those @j@ vectors.
sizedSpecialDSOMTestData :: Int -> Gen SpecialDSOMTestData
sizedSpecialDSOMTestData n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let ps = map TestPattern $ take tileCount [0,100..]
  r <- choose (0.001, 1)
  let targets = map TestPattern $ take tileCount [5,105..]
  return $ buildSpecialDSOMTestData sideLength ps r targets

instance Arbitrary SpecialDSOMTestData where
  arbitrary = sized sizedSpecialDSOMTestData

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern (which would render the test invalid).
prop_batch_training_works2 :: SpecialDSOMTestData -> Property
prop_batch_training_works2 (SpecialDSOMTestData s _ xs) =
  errBefore /= 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          errBefore = euclideanDistanceSquared (map toDouble . sort $ xs) (map toDouble . sort . models $ s)
          errAfter = euclideanDistanceSquared (map toDouble . sort $ xs) (map toDouble . sort . models $ s')

test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.DSOM"
  [
    testProperty "prop_rougierFunction_zero_if_perfect_model_exists"
      prop_rougierFunction_zero_if_perfect_model_exists,
    testProperty "prop_rougierFunction_r_if_bmu_is_bad_model"
      prop_rougierFunction_r_if_bmu_is_bad_model,
    testProperty "prop_rougierFunction_r_if_inelastic"
      prop_rougierFunction_r_if_inelastic,
    testProperty "prop_rougierFunction_r_in_bounds"
      prop_rougierFunction_r_in_bounds,
    testProperty "prop_global_instant_training_works"
      prop_global_instant_training_works,
    testProperty "prop_training_works" prop_training_works,
    testProperty "prop_classifyAndTrainEquiv"
      prop_classifyAndTrainEquiv,
    testProperty "prop_diffAndTrainEquiv" prop_diffAndTrainEquiv,
    testProperty "prop_trainNeighbourhoodEquiv" prop_trainNeighbourhoodEquiv,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_batch_training_works2"
      prop_batch_training_works2
  ]
