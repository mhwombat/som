------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMQC
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

module Data.Datamining.Clustering.SOMQC
  (
    test
  ) where

import Data.Datamining.Pattern (Pattern, Metric, difference,
  euclideanDistanceSquared, magnitudeSquared, makeSimilar)
import Data.Datamining.Clustering.Classifier(classify,
  classifyAndTrain, reportAndTrain, differences, diffAndTrain, models,
  numModels, train, trainBatch)
import Data.Datamining.Clustering.SOMInternal

import Control.Applicative
import Data.Function (on)
import Data.List (sort)
import Math.Geometry.Grid.Hexagonal (HexHexGrid, hexHexGrid)
import Math.Geometry.GridMap ((!))
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import System.Random (Random)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Gen, Arbitrary, arbitrary, choose,
  Property, property, sized, suchThat, vectorOf)

-- data GaussianArgs = GaussianArgs Double Double Int deriving Show

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

-- instance Arbitrary GaussianArgs where
--   arbitrary = GaussianArgs <$> choose (0,1) <*> positive <*> positive

-- arbDecayingGaussian :: Gen (DecayingGaussian
-- prop_decayingGaussian_small_after_tMax :: GaussianArgs -> Property
-- prop_decayingGaussian_small_after_tMax (GaussianArgs r w0 tMax) =
--   property $ decayingGaussian r w0 tMax (tMax+1) 0 < exp(-1)

-- prop_decayingGaussian_small_far_from_bmu :: GaussianArgs -> Property
-- prop_decayingGaussian_small_far_from_bmu (GaussianArgs r w0 tMax)
--   = property $
--       decayingGaussian r w0 tMax 0 (2*(ceiling w0)) < r * exp(-1)

instance
  (Random a, Num a, Ord a, Arbitrary a)
  => Arbitrary (DecayingGaussian a) where
  arbitrary = do
    r0 <- choose (0,1)
    rf <- choose (0,r0)
    w0 <- positive
    wf <- choose (0,w0)
    tf <- positive
    return $ DecayingGaussian r0 rf w0 wf tf

prop_DecayingGaussian_starts_at_r0
  :: DecayingGaussian Double -> Property
prop_DecayingGaussian_starts_at_r0 f@(DecayingGaussian r0 _ _ _ _)
  = property $ abs ((rate f 0 0) - r0) < 0.01

prop_DecayingGaussian_starts_at_w0
  :: DecayingGaussian Double -> Property
prop_DecayingGaussian_starts_at_w0 f@(DecayingGaussian r0 _ w0 _ _)
  = property $
    rate f 0 inside >= r0 * exp (-0.5) && rate f 0 outside < r0 * exp (-0.5)
  where inside = w0 - 0.001
        outside = w0 + 0.001

prop_DecayingGaussian_decays_to_rf
  :: DecayingGaussian Double -> Property
prop_DecayingGaussian_decays_to_rf f@(DecayingGaussian _ rf _ _ tf)
  = property $ abs ((rate f tf 0) - rf) < 0.01

prop_DecayingGaussian_shrinks_to_wf
  :: DecayingGaussian Double -> Property
prop_DecayingGaussian_shrinks_to_wf f@(DecayingGaussian _ rf _ wf tf)
  = property $
    rate f tf inside >= rf * exp (-0.5) && rate f tf outside < rf * exp (-0.5)
  where inside = wf - 0.001
        outside = wf + 0.001

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

fractionDiff :: [TestPattern] -> [TestPattern] -> Double
fractionDiff xs ys = if denom == 0 then 0 else d / denom
  where d = sqrt $ euclideanDistanceSquared xs' ys'
        denom = max xMag yMag
        xMag = sqrt $ magnitudeSquared xs'
        yMag = sqrt $ magnitudeSquared ys'
        xs' = map toDouble xs
        ys' = map toDouble ys

approxEqual :: [TestPattern] -> [TestPattern] -> Bool
approxEqual xs ys = fractionDiff xs ys <= 0.1

-- | A classifier and a training set. The training set will consist of
--   @j@ vectors of equal length, where @j@ is the number of patterns
--   the classifier can model. After running through the training set a
--   few times, the classifier should be very accurate at identifying
--   any of those @j@ vectors.
data SOMandTargets = SOMandTargets (SOM (DecayingGaussian Double)
  Int (LGridMap HexHexGrid) (Int, Int) TestPattern) [TestPattern]
    deriving (Eq, Show)

buildSOMandTargets
  :: Int -> [TestPattern] -> Double -> Double -> Double -> Double -> Int
     -> [TestPattern] -> SOMandTargets
buildSOMandTargets len ps r0 rf w0 wf tf targets =
  SOMandTargets s targets
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          tf' = fromIntegral tf
          s = SOM gm (DecayingGaussian r0 rf w0 wf tf') 0

sizedSOMandTargets :: Int -> Gen SOMandTargets
sizedSOMandTargets n = do
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
  return $ buildSOMandTargets sideLength ps r0 rf w0 wf tf targets

instance Arbitrary SOMandTargets where
  arbitrary = sized sizedSOMandTargets

-- | If we use a fixed learning rate of one (regardless of the distance
--   from the BMU), and train a classifier once on one pattern, then all
--   nodes should match the input vector.
prop_global_instant_training_works :: SOMandTargets -> Property
prop_global_instant_training_works (SOMandTargets s xs) =
  property $ finalModels `approxEqual` expectedModels
    where x = head xs
          gm = toGridMap s :: LGridMap HexHexGrid TestPattern
          f = (ConstantFunction 1)
          s2 = SOM gm f 0
          s3 = train s2 x
          finalModels = models s3 :: [TestPattern]
          expectedModels = replicate (numModels s) x :: [TestPattern]

prop_training_reduces_error :: SOMandTargets -> Property
prop_training_reduces_error (SOMandTargets s xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ toDouble x - toDouble (gridMap s ! bmu)
          errAfter = abs $ toDouble x - toDouble (gridMap s' ! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: SOMandTargets -> Property
prop_classifyAndTrainEquiv (SOMandTargets s ps) = property $
  bmu == s `classify` p && gridMap s1 == gridMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: SOMandTargets -> Property
prop_diffAndTrainEquiv (SOMandTargets s ps) = property $
  diffs == s `differences` p && gridMap s1 == gridMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNeighbourhood s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNeighbourhoodEquiv :: SOMandTargets -> Property
prop_trainNeighbourhoodEquiv (SOMandTargets s ps) = property $
  gridMap s1 == gridMap s2
    where p = head ps
          s1 = trainNeighbourhood s (classify s p) p
          s2 = train s p

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: SOMandTargets -> Property
prop_batch_training_works (SOMandTargets s xs) = property $
  classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
prop_classification_is_consistent
  :: SOMandTargets -> Property
prop_classification_is_consistent (SOMandTargets s (x:_))
  = property $ bmu == bmu'
  where (bmu, _, s') = reportAndTrain s x
        (bmu', _, _) = reportAndTrain s' x
prop_classification_is_consistent _ = error "Should not happen"

-- | Same as SOMandTargets, except that the initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern.
data SpecialSOMandTargets = SpecialSOMandTargets (SOM
  (StepFunction Double) Int (LGridMap HexHexGrid) (Int, Int)
  TestPattern) [TestPattern]
    deriving (Eq, Show)

buildSpecialSOMandTargets
  :: Int -> [TestPattern] -> Double -> [TestPattern] -> SpecialSOMandTargets
buildSpecialSOMandTargets len ps r targets =
  SpecialSOMandTargets s targets
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          s = SOM gm (StepFunction r) 0

sizedSpecialSOMandTargets :: Int -> Gen SpecialSOMandTargets
sizedSpecialSOMandTargets n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let ps = map MkPattern $ take tileCount [0,100..]
  r <- choose (0.001, 1)
  let targets = map MkPattern $ take tileCount [5,105..]
  return $ buildSpecialSOMandTargets sideLength ps r targets

instance Arbitrary SpecialSOMandTargets where
  arbitrary = sized sizedSpecialSOMandTargets

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern (which would render the test invalid).
prop_batch_training_works2 :: SpecialSOMandTargets -> Property
prop_batch_training_works2 (SpecialSOMandTargets s xs) =
  errBefore /= 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          errBefore = absDiff (sort xs) (sort (models s))
          errAfter = absDiff (sort xs) (sort (models s'))

data IncompleteSOMandTargets = IncompleteSOMandTargets (SOM
  (DecayingGaussian Double) Int (LGridMap HexHexGrid) (Int, Int)
  TestPattern) [TestPattern] deriving Show

buildIncompleteSOMandTargets
  :: Int -> [TestPattern] -> Double -> Double -> Double -> Double -> Int
     -> [TestPattern] -> IncompleteSOMandTargets
buildIncompleteSOMandTargets len ps r0 rf w0 wf tf targets =
  IncompleteSOMandTargets s targets
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          tf' = fromIntegral tf
          s = SOM gm (DecayingGaussian r0 rf w0 wf tf') 0

-- | Same as sizedSOMandTargets, except some nodes don't have a value.
sizedIncompleteSOMandTargets :: Int -> Gen IncompleteSOMandTargets
sizedIncompleteSOMandTargets n = do
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
  return $ buildIncompleteSOMandTargets sideLength ps r0 rf w0 wf tf targets

instance Arbitrary IncompleteSOMandTargets where
  arbitrary = sized sizedIncompleteSOMandTargets

prop_can_train_incomplete_SOM :: IncompleteSOMandTargets -> Property
prop_can_train_incomplete_SOM (IncompleteSOMandTargets s xs) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ toDouble x - toDouble (gridMap s ! bmu)
          errAfter = abs $ toDouble x - toDouble (gridMap s' ! bmu)

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
