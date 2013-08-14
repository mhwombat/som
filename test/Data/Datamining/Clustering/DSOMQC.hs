------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.DSOMQC
-- Copyright   :  (c) Amy de Buitléir 2012-2013
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

module Data.Datamining.Clustering.DSOMQC
  (
    test
  ) where

import Data.Datamining.Pattern (Pattern, Metric, difference,
  euclideanDistanceSquared, magnitudeSquared, makeSimilar)
import Data.Datamining.Clustering.Classifier(Classifier, classify,
  classifyAndTrain, differences, diffAndTrain, models,
  numModels, train, trainBatch)
import Data.Datamining.Clustering.DSOMInternal

import Control.Applicative ((<$>), (<*>))
import Data.Function (on)
import Data.List (sort)
import Math.Geometry.Grid.Hexagonal (HexHexGrid, hexHexGrid)
import Math.Geometry.GridMap ((!), GridMap)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Gen, Arbitrary, arbitrary, choose,
  Property, property, sized, suchThat, vectorOf)

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

data RougierArgs
  = RougierArgs Double Double Double Double Double deriving Show

instance Arbitrary RougierArgs where
  arbitrary = RougierArgs <$> choose (0,1) <*> choose (0,1)
                <*> arbitrary <*> choose (0,1) <*> positive

prop_rougierFunction_zero_if_perfect_model_exists :: RougierArgs -> Property
prop_rougierFunction_zero_if_perfect_model_exists (RougierArgs r p _ diff dist) =
  property $ rougierLearningFunction r p 0 diff dist == 0

prop_rougierFunction_r_if_bmu_is_bad_model :: RougierArgs -> Property
prop_rougierFunction_r_if_bmu_is_bad_model (RougierArgs r p _ _ _) =
  property $ rougierLearningFunction r p 1 1 0 == r

prop_rougierFunction_r_in_bounds :: RougierArgs -> Property
prop_rougierFunction_r_in_bounds (RougierArgs r p bmuDiff diff dist) =
  property $ 0 <= f && f <= 1
  where f = rougierLearningFunction r p bmuDiff diff dist

prop_rougierFunction_r_if_inelastic :: RougierArgs -> Property
prop_rougierFunction_r_if_inelastic (RougierArgs r _ _ _ _) =
  property $ rougierLearningFunction r 1.0 1.0 1.0 0 == r

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
  arbitrary = MkPattern <$> choose (0,1)

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

data DSOMandTargets = DSOMandTargets (DSOM (LGridMap HexHexGrid) (Int, Int)
  TestPattern) [TestPattern] String

instance Show DSOMandTargets where
  show (DSOMandTargets _ _ desc) = desc

buildDSOMandTargets
  :: Int -> [TestPattern] -> Double -> Double -> [TestPattern] -> DSOMandTargets
buildDSOMandTargets len ps r p targets = DSOMandTargets s targets desc
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          s = defaultDSOM gm r p
          desc = "buildDSOMandTargets " ++ show len ++ " " ++ show ps ++
            " " ++ show r ++ " " ++ show p ++ " " ++ show targets

-- | Generate a classifier and a training set. The training set will
--   consist @j@ vectors of equal length, where @j@ is the number of
--   patterns the classifier can model. After running through the
--   training set a few times, the classifier should be very accurate at
--   identifying any of those @j@ vectors.
sizedDSOMandTargets :: Int -> Gen DSOMandTargets
sizedDSOMandTargets n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let numberOfPatterns = tileCount
  ps <- vectorOf numberOfPatterns arbitrary
  r <- choose (0, 1)
  p <- choose (0, 1)
  targets <- vectorOf numberOfPatterns arbitrary
  return $ buildDSOMandTargets sideLength ps r p targets

instance Arbitrary DSOMandTargets where
  arbitrary = sized sizedDSOMandTargets

-- | If we use a fixed learning rate of one (regardless of the distance
--   from the BMU), and train a classifier once on one pattern, then all
--   nodes should match the input vector.
prop_global_instant_training_works :: DSOMandTargets -> Property
prop_global_instant_training_works (DSOMandTargets s xs _) =
  property $ finalModels `approxEqual` expectedModels
    where x = head xs
          gm = toGridMap s :: LGridMap HexHexGrid TestPattern
          f = (\_ _ _ -> 1) 
              :: Metric TestPattern -> Metric TestPattern -> Metric TestPattern -> Metric TestPattern
          s2 = customDSOM gm f :: DSOM (LGridMap HexHexGrid) (Int, Int) TestPattern
          s3 = train s2 x
          finalModels = models s3 :: [TestPattern]
          expectedModels = replicate (numModels s) x :: [TestPattern]

prop_training_works :: DSOMandTargets -> Property
prop_training_works (DSOMandTargets s xs _) = errBefore /= 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ toDouble x - toDouble (sGridMap s ! bmu)
          errAfter = abs $ toDouble x - toDouble (sGridMap s' ! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv :: DSOMandTargets -> Property
prop_classifyAndTrainEquiv (DSOMandTargets s ps _) = property $
  bmu == s `classify` p && sGridMap s1 == sGridMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv :: DSOMandTargets -> Property
prop_diffAndTrainEquiv (DSOMandTargets s ps _) = property $
  diffs == s `differences` p && sGridMap s1 == sGridMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNeighbourhood s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNeighbourhoodEquiv :: DSOMandTargets -> Property
prop_trainNeighbourhoodEquiv (DSOMandTargets s ps _) = property $
  sGridMap s1 == sGridMap s2
    where p = head ps
          s1 = trainNeighbourhood s (classify s p) p
          s2 = train s p

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works :: DSOMandTargets -> Property
prop_batch_training_works (DSOMandTargets s xs _) = property $
  classifications == (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

data SpecialDSOMandTargets = SpecialDSOMandTargets (DSOM (LGridMap HexHexGrid) (Int, Int)
  TestPattern) [TestPattern] String

instance Show SpecialDSOMandTargets where
  show (SpecialDSOMandTargets _ _ desc) = desc

stepFunction :: Double -> Double -> Double -> Double -> Double
stepFunction r _ _ d = if d == 0 then r else 0.0

buildSpecialDSOMandTargets
  :: Int -> [TestPattern] -> Double -> [TestPattern] -> SpecialDSOMandTargets
buildSpecialDSOMandTargets len ps r targets =
  SpecialDSOMandTargets s targets desc
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          s = customDSOM gm (stepFunction r)
          desc = "buildSpecialDSOMandTargets " ++ show len ++ " "
            ++ show ps ++ " " ++ show r ++ " " ++ show targets

-- | Generate a classifier and a training set. The training set will
--   consist @j@ vectors of equal length, where @j@ is the number of
--   patterns the classifier can model. After running through the
--   training set a few times, the classifier should be very accurate at
--   identifying any of those @j@ vectors.
sizedSpecialDSOMandTargets :: Int -> Gen SpecialDSOMandTargets
sizedSpecialDSOMandTargets n = do
  sideLength <- choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let ps = map MkPattern . take tileCount $ [0,100..]
  r <- choose (0.001, 1)
  let targets = map MkPattern . take tileCount $ [5,105..]
  return $ buildSpecialDSOMandTargets sideLength ps r targets

instance Arbitrary SpecialDSOMandTargets where
  arbitrary = sized sizedSpecialDSOMandTargets

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The initial models and training
--   set are designed to ensure that a single node will NOT train to
--   more than one pattern (which would render the test invalid).
prop_batch_training_works2 :: SpecialDSOMandTargets -> Property
prop_batch_training_works2 (SpecialDSOMandTargets s xs _) =
  errBefore /= 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          errBefore = absDiff (sort xs) (sort (models s))
          errAfter = absDiff (sort xs) (sort (models s'))

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
