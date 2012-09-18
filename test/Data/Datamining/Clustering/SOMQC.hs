{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}

module Data.Datamining.Clustering.SOMQC
  (
    test
  ) where

import Data.Datamining.Clustering.SOM
import Data.Datamining.Clustering.SOMInternal

import Control.Applicative ((<$>), (<*>))
import Data.Eq.Unicode ((≡), (≠))
import Data.Ord.Unicode ((≤))
import Data.List (sort)
import Math.Geometry.Grid (Grid, HexHexGrid, hexHexGrid, tileCount)
import Math.Geometry.GridMap ((!), lazyGridMap, GridMap, elems)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Gen, Arbitrary, arbitrary, choose, Property, 
  property, sized, vector, vectorOf)

newtype UnitInterval = FromDouble Double deriving Show

instance Arbitrary UnitInterval where
  arbitrary = FromDouble <$> choose (0,1)

prop_adjustVector_doesnt_choke_on_infinite_lists ∷
  [Double] → UnitInterval → Property
prop_adjustVector_doesnt_choke_on_infinite_lists xs (FromDouble d) = 
  property $ 
    length (adjustVector xs d [0,1..]) ≡ length xs

data TwoVectorsSameLength = TwoVectorsSameLength [Double] [Double] 
  deriving Show

sizedTwoVectorsSameLength ∷ Int → Gen TwoVectorsSameLength
sizedTwoVectorsSameLength n = TwoVectorsSameLength <$> vector n <*> vector n

instance Arbitrary TwoVectorsSameLength where
  arbitrary = sized sizedTwoVectorsSameLength

prop_adjustVector_zero_doesnt_change_input ∷ TwoVectorsSameLength → Property
prop_adjustVector_zero_doesnt_change_input (TwoVectorsSameLength xs ys) = 
  property $ adjustVector xs 0 ys ≡ ys

prop_adjustVector_one_gives_perfect_match ∷ TwoVectorsSameLength → Property
prop_adjustVector_one_gives_perfect_match (TwoVectorsSameLength xs ys) = 
  property $ adjustVector xs 1 ys ≡ xs

prop_adjustVector_improves_similarity ∷ 
  TwoVectorsSameLength → UnitInterval → Property
prop_adjustVector_improves_similarity 
  (TwoVectorsSameLength xs ys) (FromDouble a) = 
    a > 0 && a < 1 && not (null xs) ==> d2 < d1
      where d1 = euclideanDistanceSquared xs ys
            d2 = euclideanDistanceSquared xs ys'
            ys' = adjustVector xs a ys

instance Pattern Double Double where
  difference x y = abs (y-x)
  makeSimilar x r y = y + r*(x-y)

data ClassifierAndTargets = ClassifierAndTargets 
  (GridMap HexHexGrid (Int,Int) Double) [Double] 
    deriving Show

-- | Generate a classifier and a training set. The training set will consist
--   @j@ vectors of equal length, where @j@ is the number of patterns the
--   classifier can model. After running through the training set a few times,
--   the classifier should be very accurate at identifying any of those @j@
--   vectors.
sizedClassifierAndTargets ∷ Int → Gen ClassifierAndTargets
sizedClassifierAndTargets n = do
  sideLength ← choose (1, min (n+1) 5) --don't want the test to take too long
  let g = hexHexGrid sideLength
  let numberOfPatterns = tileCount g
  ps ← vectorOf numberOfPatterns arbitrary
  let c = lazyGridMap g ps
  targets ← vectorOf numberOfPatterns arbitrary
  return $ ClassifierAndTargets c targets
  
instance Arbitrary ClassifierAndTargets where
  arbitrary = sized sizedClassifierAndTargets

fractionDiff ∷ (Floating a, Ord a) ⇒ [a] → [a] → a
fractionDiff xs ys = if denom ≡ 0 then 0 else d / denom
  where d = sqrt $ euclideanDistanceSquared xs ys
        denom = max xMag yMag
        xMag = sqrt $ magnitudeSquared xs
        yMag = sqrt $ magnitudeSquared ys

approxEqual ∷ (Floating a, Ord a) ⇒ [a] → [a] → Bool
approxEqual xs ys = fractionDiff xs ys ≤ 0.1

-- | If we use a fixed learning rate of one (regardless of the distance from
--   the BMU), and train a classifier once on one pattern, then all nodes
--   should match the input vector.
prop_global_instant_training_works ∷ ClassifierAndTargets → Property
prop_global_instant_training_works (ClassifierAndTargets c xs) = property $
  elems c' `approxEqual` replicate (tileCount c) x
    where x = head xs
          c' = train (\_ → 1.0) c x

prop_training_works ∷ ClassifierAndTargets → Property
prop_training_works (ClassifierAndTargets c xs) = errBefore ≠ 0 ==>
  errAfter < errBefore
    where (bmu, c') = classifyAndTrain (gaussian 0.5 0.8) c x
          x = head xs
          errBefore = abs (x - c ! bmu)
          errAfter = abs (x - c' ! bmu)

-- | The training set consists of the same vectors in the same order, several
--   times over. So the resulting classifications should consist of the same
--   integers in the same order, over and over.
prop_batch_training_works ∷ ClassifierAndTargets → Property
prop_batch_training_works (ClassifierAndTargets c xs) = property $
  classifications ≡ (concat . replicate 5) firstSet 
  where trainingSet = (concat . replicate 5) xs
        c' = trainBatch (gaussian 0.5 0.8) c trainingSet
        classifications = map (classify c') trainingSet
        firstSet = take (length xs) classifications

-- | If we train a classifier once on a set of patterns, where the 
--   number of patterns in the set is equal to the number of nodes in the 
--   classifier, then the classifier should become a better representation of
--   the training set. The training set is designed to reduce the possibility
--   that a single node will train to more than one pattern, rendering the
--   test invalid.
prop_batch_training_works2 ∷ ClassifierAndTargets → Property
prop_batch_training_works2 (ClassifierAndTargets c _) = errBefore ≠ 0 ==>
  errAfter < errBefore
    where c' = trainBatch (gaussian 0.5 0.8) c xs
          xs = take (tileCount c) [0,100..]
          errBefore = euclideanDistanceSquared (sort xs) (sort (elems c))
          errAfter = euclideanDistanceSquared (sort xs) (sort (elems c'))

test ∷ Test
test = testGroup "QuickCheck Data.Datamining.Clustering.SOMQC"
  [
    testProperty "prop_adjustVector_doesnt_choke_on_infinite_lists"
      prop_adjustVector_doesnt_choke_on_infinite_lists,
    testProperty "prop_adjustVector_zero_doesnt_change_input"
      prop_adjustVector_zero_doesnt_change_input,
    testProperty "prop_adjustVector_one_gives_perfect_match"
      prop_adjustVector_one_gives_perfect_match,
    testProperty "prop_adjustVector_improves_similarity"
      prop_adjustVector_improves_similarity,
    testProperty "prop_global_instant_training_works"
      prop_global_instant_training_works,
    testProperty "prop_training_works"
      prop_training_works,
    testProperty "prop_batch_training_works"
      prop_batch_training_works,
    testProperty "prop_batch_training_works2"
      prop_batch_training_works2
  ]

