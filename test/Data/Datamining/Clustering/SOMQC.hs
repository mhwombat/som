------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMQC
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, TypeFamilies,
    FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}

module Data.Datamining.Clustering.SOMQC
  (
    test
  ) where

import Data.Datamining.Pattern (Pattern, Metric, difference,
  euclideanDistanceSquared, magnitudeSquared, makeSimilar)
import Data.Datamining.Clustering.Classifier(Classifier, classify,
  classifyAndTrain, differences, diffAndTrain, models,
  numModels, train, trainBatch)
import Data.Datamining.Clustering.SOMInternal

import Control.Applicative ((<$>), (<*>))
import Data.Function (on)
import Data.Eq.Unicode ((≡), (≠))
import Data.Ord.Unicode ((≤))
import Data.List (sort)
import Math.Geometry.Grid.Hexagonal (HexHexGrid, hexHexGrid)
import Math.Geometry.GridMap ((!), GridMap)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Gen, Arbitrary, arbitrary, choose,
  Property, property, sized, suchThat, vectorOf)

data GaussianArgs = GaussianArgs Double Double Int deriving Show

positive :: (Num a, Ord a, Arbitrary a) => Gen a
positive = arbitrary `suchThat` (> 0)

instance Arbitrary GaussianArgs where
  arbitrary = GaussianArgs <$> choose (0,1) <*> positive <*> positive

prop_decayingGaussian_small_after_tMax :: GaussianArgs -> Property
prop_decayingGaussian_small_after_tMax (GaussianArgs r w0 tMax) =
  property $ decayingGaussian r w0 tMax (tMax+1) 0 < exp(-1)

prop_decayingGaussian_small_far_from_bmu :: GaussianArgs -> Property
prop_decayingGaussian_small_far_from_bmu (GaussianArgs r w0 tMax)
  = property $
      decayingGaussian r w0 tMax 0 (2*(ceiling w0)) < r * exp(-1)

data GaussianArgs2 = GaussianArgs2 Double Double Double Double Int
                       deriving Show

instance Arbitrary GaussianArgs2 where
  arbitrary = do
    r0 <- choose (0,1)
    rf <- choose (0,r0)
    w0 <- positive
    wf <- choose (0,w0)
    tf <- positive
    return $ GaussianArgs2 r0 rf w0 wf tf

prop_decayingGaussian2_starts_at_r0 :: GaussianArgs2 -> Property
prop_decayingGaussian2_starts_at_r0 (GaussianArgs2 r0 rf w0 wf tf) =
  property $ abs ((decayingGaussian2 r0 rf w0 wf tf 0 0) - r0) < 0.01

prop_decayingGaussian2_starts_at_w0 :: GaussianArgs2 -> Property
prop_decayingGaussian2_starts_at_w0 (GaussianArgs2 r0 rf w0 wf tf) =
  property $
    decayingGaussian2 r0 rf w0 wf tf 0 inside >= r0 * exp (-1)
    && decayingGaussian2 r0 rf w0 wf tf 0 outside < r0 * exp (-1)
  where inside = floor w0
        outside = ceiling w0 + 1

prop_decayingGaussian2_decays_to_rf :: GaussianArgs2 -> Property
prop_decayingGaussian2_decays_to_rf (GaussianArgs2 r0 rf w0 wf tf) =
  property $ abs ((decayingGaussian2 r0 rf w0 wf tf tf 0) - rf) < 0.01

prop_decayingGaussian2_shrinks_to_wf :: GaussianArgs2 -> Property
prop_decayingGaussian2_shrinks_to_wf (GaussianArgs2 r0 rf w0 wf tf) =
  property $
  decayingGaussian2 r0 rf w0 wf tf tf inside >= rf * exp (-1)
    && decayingGaussian2 r0 rf w0 wf tf tf outside < rf * exp (-1)
  where inside = floor wf
        outside = ceiling wf + 1


newtype TestPattern = MkPattern Double deriving Show

instance Eq TestPattern where
  (==) = (==) `on` toDouble

instance Ord TestPattern where
  compare = compare `on` toDouble

instance Pattern TestPattern where
  type Metric TestPattern = Double
  difference (MkPattern a) (MkPattern b) = a - b
  makeSimilar orig@(MkPattern a) r (MkPattern b)
    | r < 0     = error "Negative learning rate"
    | r > 1     = error "Learning rate > 1"
    | r ≡ 1     = orig
    | otherwise = MkPattern (b + delta)
        where diff = a - b
              delta = r*diff

instance Arbitrary TestPattern where
  arbitrary = MkPattern <$> arbitrary

toDouble ∷ TestPattern → Double
toDouble (MkPattern a) = a

absDiff ∷ [TestPattern] → [TestPattern] → Double
absDiff xs ys = euclideanDistanceSquared xs' ys'
  where xs' = map toDouble xs
        ys' = map toDouble ys

fractionDiff ∷ [TestPattern] → [TestPattern] → Double
fractionDiff xs ys = if denom ≡ 0 then 0 else d / denom
  where d = sqrt $ euclideanDistanceSquared xs' ys'
        denom = max xMag yMag
        xMag = sqrt $ magnitudeSquared xs'
        yMag = sqrt $ magnitudeSquared ys'
        xs' = map toDouble xs
        ys' = map toDouble ys

approxEqual ∷ [TestPattern] → [TestPattern] → Bool
approxEqual xs ys = fractionDiff xs ys ≤ 0.1

data SOMandTargets = SOMandTargets (SOM (LGridMap HexHexGrid) (Int, Int)
  TestPattern) [TestPattern] String

instance Show SOMandTargets where
  show (SOMandTargets _ _ desc) = desc

buildSOMandTargets
  ∷ Int → [TestPattern] → Double → Double → Int → [TestPattern] → SOMandTargets
buildSOMandTargets len ps r w t targets = SOMandTargets s targets desc
    where g = hexHexGrid len
          gm = lazyGridMap g ps
          s = defaultSOM gm r w t
          desc = "buildSOMandTargets " ++ show len ++ " " ++ show ps ++
            " " ++ show r ++ " " ++ show w ++ " " ++ show t ++ " " ++
            show targets

-- | Generate a classifier and a training set. The training set will
--   consist @j@ vectors of equal length, where @j@ is the number of
--   patterns the classifier can model. After running through the
--   training set a few times, the classifier should be very accurate at
--   identifying any of those @j@ vectors.
sizedSOMandTargets ∷ Int → Gen SOMandTargets
sizedSOMandTargets n = do
  sideLength ← choose (1, min (n+1) 5) --avoid long tests
  let tileCount = 3*sideLength*(sideLength-1) + 1
  let numberOfPatterns = tileCount
  ps ← vectorOf numberOfPatterns arbitrary
  r ← choose (0, 1)
  w0 ← choose (0, 2)
  tMax ← choose (1, 10)
  targets ← vectorOf numberOfPatterns arbitrary
  return $ buildSOMandTargets sideLength ps r w0 tMax targets

instance Arbitrary SOMandTargets where
  arbitrary = sized sizedSOMandTargets

-- | If we use a fixed learning rate of one (regardless of the distance
--   from the BMU), and train a classifier once on one pattern, then all
--   nodes should match the input vector.
prop_global_instant_training_works ∷ SOMandTargets → Property
prop_global_instant_training_works (SOMandTargets s xs _) =
  property $ finalModels `approxEqual` expectedModels
    where x = head xs
          gm = toGridMap s ∷ LGridMap HexHexGrid TestPattern
          f = (\_ _ → 1) ∷ Int → Int → Metric TestPattern
          s2 = customSOM gm f ∷ SOM (LGridMap HexHexGrid) (Int, Int) TestPattern
          s3 = train s2 x
          finalModels = models s3 ∷ [TestPattern]
          expectedModels = replicate (numModels s) x ∷ [TestPattern]

prop_training_works ∷ SOMandTargets → Property
prop_training_works (SOMandTargets s xs _) = errBefore ≠ 0 ==>
  errAfter < errBefore
    where (bmu, s') = classifyAndTrain s x
          x = head xs
          errBefore = abs $ toDouble x - toDouble (sGridMap s ! bmu)
          errAfter = abs $ toDouble x - toDouble (sGridMap s' ! bmu)

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(p `classify` s, train s f p)@.
prop_classifyAndTrainEquiv ∷ SOMandTargets → Property
prop_classifyAndTrainEquiv (SOMandTargets s ps _) = property $
  bmu ≡ s `classify` p && sGridMap s1 ≡ sGridMap s2
    where p = head ps
          (bmu, s1) = classifyAndTrain s p
          s2 = train s p

--   Invoking @diffAndTrain f s p@ should give identical results to
--   @(s `diff` p, train s f p)@.
prop_diffAndTrainEquiv ∷ SOMandTargets → Property
prop_diffAndTrainEquiv (SOMandTargets s ps _) = property $
  diffs ≡ s `differences` p && sGridMap s1 ≡ sGridMap s2
    where p = head ps
          (diffs, s1) = diffAndTrain s p
          s2 = train s p

--   Invoking @trainNeighbourhood s (classify s p) p@ should give
--   identical results to @train s p@.
prop_trainNeighbourhoodEquiv ∷ SOMandTargets → Property
prop_trainNeighbourhoodEquiv (SOMandTargets s ps _) = property $
  sGridMap s1 ≡ sGridMap s2
    where p = head ps
          s1 = trainNeighbourhood s (classify s p) p
          s2 = train s p

-- | The training set consists of the same vectors in the same order,
--   several times over. So the resulting classifications should consist
--   of the same integers in the same order, over and over.
prop_batch_training_works ∷ SOMandTargets → Property
prop_batch_training_works (SOMandTargets s xs _) = property $
  classifications ≡ (concat . replicate 5) firstSet
  where trainingSet = (concat . replicate 5) xs
        s' = trainBatch s trainingSet
        classifications = map (classify s') trainingSet
        firstSet = take (length xs) classifications

-- | If we train a classifier once on a set of patterns, where the
--   number of patterns in the set is equal to the number of nodes in
--   the classifier, then the classifier should become a better
--   representation of the training set. The training set is designed*
--   to reduce the possibility that a single node will train to more
--   than one pattern (which would render the test invalid).
prop_batch_training_works2 ∷ SOMandTargets → Property
prop_batch_training_works2 (SOMandTargets s _ _) =
  errBefore ≠ 0 ==> errAfter < errBefore
    where s' = trainBatch s xs
          xs = map MkPattern $ take (numModels s) [0,1000..] -- see *
          errBefore = absDiff (sort xs) (sort (models s))
          errAfter = absDiff (sort xs) (sort (models s'))

test ∷ Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SOM.ClassifierQC"
  [
    testProperty "prop_decayingGaussian_small_after_tMax"
      prop_decayingGaussian_small_after_tMax,
    testProperty "prop_decayingGaussian_small_far_from_bmu"
      prop_decayingGaussian_small_far_from_bmu,
    testProperty "prop_decayingGaussian2_starts_at_r0"
      prop_decayingGaussian2_starts_at_r0,
    testProperty "prop_decayingGaussian2_starts_at_w0"
      prop_decayingGaussian2_starts_at_w0,
    testProperty "prop_decayingGaussian2_decays_to_rf"
      prop_decayingGaussian2_decays_to_rf,
    testProperty "prop_decayingGaussian2_shrinks_to_wf"
      prop_decayingGaussian2_shrinks_to_wf,
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
