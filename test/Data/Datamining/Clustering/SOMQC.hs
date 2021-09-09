------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMQC
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

module Data.Datamining.Clustering.SOMQC
  (
    test
  ) where

import           Data.Datamining.Clustering.Classifier  (classify,
                                                         classifyAndTrain,
                                                         diffAndTrain,
                                                         differences, models,
                                                         numModels,
                                                         reportAndTrain, train,
                                                         trainBatch)
import           Data.Datamining.Clustering.SOMInternal
import           Data.Datamining.Pattern                (absDifference,
                                                         adjustNum,
                                                         euclideanDistanceSquared,
                                                         magnitudeSquared)

import           Data.List                              (sort)
import           Math.Geometry.Grid                     (size)
import           Math.Geometry.Grid.Hexagonal           (HexHexGrid, hexHexGrid)
import           Math.Geometry.GridMap                  (elems, (!))
import           Math.Geometry.GridMap.Lazy             (LGridMap, lazyGridMap)
import           System.Random                          (Random)
import           Test.Framework                         as TF (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2   (testProperty)
import           Test.QuickCheck                        (Arbitrary, Gen,
                                                         Property, arbitrary,
                                                         choose, property,
                                                         sized, suchThat,
                                                         vectorOf, (==>))

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

data DecayingGaussianParams a = DecayingGaussianParams a a a a a
  deriving (Eq, Show)

instance
  (Random a, Num a, Ord a, Arbitrary a)
  => Arbitrary (DecayingGaussianParams a) where
  arbitrary = do
    r0 <- choose (0,1)
    rf <- choose (0,r0)
    w0 <- positive
    wf <- choose (0,w0)
    tf <- positive
    return $ DecayingGaussianParams r0 rf w0 wf tf

prop_DecayingGaussian_starts_at_r0
  :: DecayingGaussianParams Double -> Property
prop_DecayingGaussian_starts_at_r0 (DecayingGaussianParams r0 rf w0 wf tf)
  = property $ abs (decayingGaussian r0 rf w0 wf tf 0 0 - r0) < 0.01

prop_DecayingGaussian_starts_at_w0
  :: DecayingGaussianParams Double -> Property
prop_DecayingGaussian_starts_at_w0 (DecayingGaussianParams r0 rf w0 wf tf)
  = property $
    decayingGaussian r0 rf w0 wf tf 0 inside >= r0 * exp (-0.5)
      && decayingGaussian r0 rf w0 wf tf 0 outside < r0 * exp (-0.5)
  where inside = w0 * 0.99999
        outside = w0 * 1.00001

prop_DecayingGaussian_decays_to_rf
  :: DecayingGaussianParams Double -> Property
prop_DecayingGaussian_decays_to_rf (DecayingGaussianParams r0 rf w0 wf tf)
  = property $ abs (decayingGaussian r0 rf w0 wf tf tf 0 - rf) < 0.01

prop_DecayingGaussian_shrinks_to_wf
  :: DecayingGaussianParams Double -> Property
prop_DecayingGaussian_shrinks_to_wf (DecayingGaussianParams r0 rf w0 wf tf)
  = property $
    decayingGaussian r0 rf w0 wf tf tf inside >= rf * exp (-0.5)
      && decayingGaussian r0 rf w0 wf tf tf outside < rf * exp (-0.5)
  where inside = wf * 0.99999
        outside = wf * 1.00001

fractionDiff :: [Double] -> [Double] -> Double
fractionDiff xs ys = if denom == 0 then 0 else d / denom
  where d = sqrt $ euclideanDistanceSquared xs ys
        denom = max xMag yMag
        xMag = sqrt $ magnitudeSquared xs
        yMag = sqrt $ magnitudeSquared ys

approxEqual :: [Double] -> [Double] -> Bool
approxEqual xs ys = fractionDiff xs ys <= 0.1

-- | A classifier and a training set. The training set will consist of
--   @j@ vectors of equal length, where @j@ is the number of patterns
--   the classifier can model. After running through the training set a
--   few times, the classifier should be very accurate at identifying
--   any of those @j@ vectors.
data SOMTestData
  = SOMTestData
    {
      som1 :: SOM Double Double (LGridMap HexHexGrid) Double (Int, Int) Double,
      params1 :: DecayingGaussianParams Double,
      trainingSet1 :: [Double]
    }

instance Show SOMTestData where
  show s = "buildSOMTestData " ++ show (size . gridMap . som1 $ s)
    ++ " " ++ show (elems . gridMap . som1 $ s)
    ++ " (" ++ show (params1 s)
    ++ ") " ++ show (trainingSet1 s)

buildSOMTestData
  :: Int -> [Double] -> DecayingGaussianParams Double
     -> [Double] -> SOMTestData
buildSOMTestData len ps p@(DecayingGaussianParams r0 rf w0 wf tf) =
  SOMTestData s p
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          fr = decayingGaussian r0 rf w0 wf tf
          s = SOM gm fr absDifference adjustNum 0

sizedSOMTestData :: Int -> Gen SOMTestData
sizedSOMTestData n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let numberOfPatterns = tileCount
  ps <- vectorOf numberOfPatterns arbitrary
  r0 <- choose (0, 1)
  rf <- choose (0, r0)
  w0 <- choose (0, fromIntegral sideLength)
  wf <- choose (0, w0)
  tf <- choose (1, 10)
  targets <- vectorOf numberOfPatterns arbitrary
  return $ buildSOMTestData sideLength ps (DecayingGaussianParams r0 rf w0 wf tf) targets

instance Arbitrary SOMTestData where
  arbitrary = sized sizedSOMTestData

-- | If we use a fixed learning rate of one (regardless of the distance
--   from the BMU), and train a classifier once on one pattern, then all
--   nodes should match the input vector.
prop_global_instant_training_works :: SOMTestData -> Property
prop_global_instant_training_works (SOMTestData s _ xs) =
  property $ finalModels `approxEqual` expectedModels
    where x = head xs
          gm = toGridMap s :: LGridMap HexHexGrid Double
          f _ _ = 1
          s2 = SOM gm f absDifference adjustNum 0
          s3 = train s2 x
          finalModels = models s3 :: [Double]
          expectedModels = replicate (numModels s) x :: [Double]

prop_training_reduces_error :: SOMTestData -> Property
prop_training_reduces_error (SOMTestData s _ xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ x - (gridMap s ! bmu)
          errAfter = abs $ x - (gridMap s' ! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: SOMTestData -> Property
prop_classifyAndTrainEquiv (SOMTestData s _ ps) = property $
  bmu == s `classify` p && gridMap s1 == gridMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: SOMTestData -> Property
prop_diffAndTrainEquiv (SOMTestData s _ ps) = property $
  diffs == s `differences` p && gridMap s1 == gridMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNeighbourhood s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNeighbourhoodEquiv :: SOMTestData -> Property
prop_trainNeighbourhoodEquiv (SOMTestData s _ ps) = property $
  gridMap s1 == gridMap s2
    where p = head ps
          s1 = trainNeighbourhood s (classify s p) p
          s2 = train s p

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: SOMTestData -> Property
prop_batch_training_works (SOMTestData s _ xs) = property $
  classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
--   This only happens rarely, so if the test fails, try again.
prop_classification_is_consistent
  :: SOMTestData -> Property
prop_classification_is_consistent (SOMTestData s _ (x:_))
  = property $ bmu == bmu'
  where (bmu, _, s') = reportAndTrain s x
        (bmu', _, _) = reportAndTrain s' x
prop_classification_is_consistent _ = error "Should not happen"

-- | Same as SOMTestData, except that the initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern.
data SpecialSOMTestData
  = SpecialSOMTestData
    {
      som2         :: SOM Int Int (LGridMap HexHexGrid) Double (Int, Int) Double,
      params2      :: Double,
      trainingSet2 :: [Double]
    }

instance Show SpecialSOMTestData where
  show s = "buildSpecialSOMTestData " ++ show (size . gridMap . som2 $ s)
    ++ " " ++ show (elems . gridMap . som2 $ s)
    ++ " " ++ show (params2 s)
    ++ " " ++ show (trainingSet2 s)

buildSpecialSOMTestData
  :: Int -> [Double] -> Double -> [Double] -> SpecialSOMTestData
buildSpecialSOMTestData len ps r targets =
  SpecialSOMTestData s r targets
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          s = SOM gm (stepFunction r) absDifference adjustNum 0

sizedSpecialSOMTestData :: Int -> Gen SpecialSOMTestData
sizedSpecialSOMTestData n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let ps = take tileCount [0,100..]
  r <- choose (0.001, 1)
  let targets = take tileCount [5,105..]
  return $ buildSpecialSOMTestData sideLength ps r targets

instance Arbitrary SpecialSOMTestData where
  arbitrary = sized sizedSpecialSOMTestData

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern (which would render the test invalid).
prop_batch_training_works2 :: SpecialSOMTestData -> Property
prop_batch_training_works2 (SpecialSOMTestData s _ xs) =
  errBefore /= 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          errBefore = euclideanDistanceSquared (sort xs) (sort (models s))
          errAfter = euclideanDistanceSquared (sort xs) (sort (models s'))

data IncompleteSOMTestData
  = IncompleteSOMTestData
    {
      som3 :: SOM Double Double (LGridMap HexHexGrid) Double (Int, Int) Double,
      params3 :: DecayingGaussianParams Double,
      trainingSet3 :: [Double]
    }

instance Show IncompleteSOMTestData where
  show s = "buildIncompleteSOMTestData " ++ show (size . gridMap . som3 $ s)
    ++ " " ++ show (elems . gridMap . som3 $ s)
    ++ " " ++ show (params3 s)
    ++ " " ++ show (trainingSet3 s)

buildIncompleteSOMTestData
  :: Int -> [Double] -> DecayingGaussianParams Double
     -> [Double] -> IncompleteSOMTestData
buildIncompleteSOMTestData len ps p@(DecayingGaussianParams r0 rf w0 wf tf) =
  IncompleteSOMTestData s p
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          fr = decayingGaussian r0 rf w0 wf tf
          s = SOM gm fr absDifference adjustNum 0

-- | Same as sizedSOMTestData, except some nodes don't have a value.
sizedIncompleteSOMTestData :: Int -> Gen IncompleteSOMTestData
sizedIncompleteSOMTestData n = do
  sideLength <- choose (2, min (n+2) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  numberOfPatterns <- choose (1,tileCount-1)
  ps <- vectorOf numberOfPatterns arbitrary
  r0 <- choose (0, 1)
  rf <- choose (0, r0)
  w0 <- choose (0, fromIntegral sideLength)
  wf <- choose (0, w0)
  tf <- choose (1, 10)
  targets <- vectorOf numberOfPatterns arbitrary
  return $ buildIncompleteSOMTestData sideLength ps (DecayingGaussianParams r0 rf w0 wf tf) targets

instance Arbitrary IncompleteSOMTestData where
  arbitrary = sized sizedIncompleteSOMTestData

prop_can_train_incomplete_SOM :: IncompleteSOMTestData -> Property
prop_can_train_incomplete_SOM (IncompleteSOMTestData s _ xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ x - (gridMap s ! bmu)
          errAfter = abs $ x - (gridMap s' ! bmu)

test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SOM"
  [
    testProperty "prop_DecayingGaussian_starts_at_r0"
      prop_DecayingGaussian_starts_at_r0,
    testProperty "prop_DecayingGaussian_starts_at_w0"
      prop_DecayingGaussian_starts_at_w0,
    testProperty "prop_DecayingGaussian_decays_to_rf"
      prop_DecayingGaussian_decays_to_rf,
    testProperty "prop_DecayingGaussian_shrinks_to_wf"
      prop_DecayingGaussian_shrinks_to_wf,
    testProperty "prop_global_instant_training_works"
      prop_global_instant_training_works,
    testProperty "prop_training_reduces_error"
      prop_training_reduces_error,
    testProperty "prop_classifyAndTrainEquiv"
      prop_classifyAndTrainEquiv,
    testProperty "prop_diffAndTrainEquiv" prop_diffAndTrainEquiv,
    testProperty "prop_trainNeighbourhoodEquiv" prop_trainNeighbourhoodEquiv,
    testProperty "prop_batch_training_works" prop_batch_training_works,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent,
    testProperty "prop_batch_training_works2"
      prop_batch_training_works2,
    testProperty "prop_can_train_incomplete_SOM"
      prop_can_train_incomplete_SOM
  ]
