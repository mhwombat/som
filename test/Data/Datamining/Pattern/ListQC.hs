------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.ListQC
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module Data.Datamining.Pattern.ListQC
  (
    test,
    UnitInterval(FromDouble),
  ) where

import qualified Data.Datamining.Pattern.List         as L
import           Data.Datamining.Pattern.Numeric      (doubleDiff,
                                                       makeOrdFractionalSimilar,
                                                       maxDouble, minDouble)
import           Data.Datamining.Pattern.NumericQC    (UnitInterval (FromDouble))
import qualified Numeric.ApproxEq                     as Q
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

data TestWeights = TestWeights [Double] deriving Show

sizedTestWeights :: Int -> Gen TestWeights
sizedTestWeights = fmap (TestWeights . normalise . map abs) . vector

instance Arbitrary TestWeights where
  arbitrary = sized sizedTestWeights

normalise :: [Double] -> [Double]
normalise xs
  | x > 0     = map (/x) xs
  | otherwise = replicate l (1 / fromIntegral l)
  where x = sum xs
        l = length xs

-- | Weights and vectors that are all the same length
data TestData = TestData [Double] [Double] [Double]
  deriving Show

sizedTestData :: Int -> Gen TestData
sizedTestData n = do
  TestWeights ws <- sizedTestWeights (n+1)
  xs <- vector (n+1)
  ys <- vector (n+1)
  return $ TestData ws xs ys

instance Arbitrary TestData where
  arbitrary = sized sizedTestData

prop_diff_can_be_0 :: [Double] -> Bool
prop_diff_can_be_0 xs = L.diff doubleDiff xs xs == 0

prop_weightedDiff_can_be_0 :: TestData -> Bool
prop_weightedDiff_can_be_0 (TestData ws xs _)
  = L.weightedDiff ws doubleDiff xs xs == 0

arbitraryMaximallyDifferentPair :: Gen (Double, Double)
arbitraryMaximallyDifferentPair = do
  swap <- arbitrary
  if swap
    then return (minDouble, maxDouble)
    else return (maxDouble, minDouble)

-- | Weights and vectors that are all the same length, and where the
--   vectors are maximally different.
data MaximallyDifferentTestData = MDTestData [Double] [Double] [Double]
  deriving Show

sizedMDTestData :: Int -> Gen MaximallyDifferentTestData
sizedMDTestData n = do
  TestWeights ws <- sizedTestWeights (n+1)
  (xs, ys) <- fmap unzip $ vectorOf (n+1) arbitraryMaximallyDifferentPair
  return $ MDTestData ws xs ys

instance Arbitrary MaximallyDifferentTestData where
  arbitrary = sized sizedMDTestData

prop_diff_can_be_1 :: MaximallyDifferentTestData -> Bool
prop_diff_can_be_1 (MDTestData _ xs ys)
  = Q.within 20 (L.diff doubleDiff xs ys) 1

prop_weightedDiff_can_be_1 :: MaximallyDifferentTestData -> Bool
prop_weightedDiff_can_be_1 (MDTestData ws xs ys)
  = Q.within 20 (L.weightedDiff ws doubleDiff xs ys) 1

prop_diff_btw_0_and_1 :: [Double] -> [Double] -> Bool
prop_diff_btw_0_and_1 xs ys = 0 <= z && z <= 1
  where z = L.diff doubleDiff xs ys

prop_weightedDiff_btw_0_and_1 :: TestWeights -> [Double] -> [Double] -> Bool
prop_weightedDiff_btw_0_and_1 (TestWeights ws) xs ys = 0 <= z && z <= 1
  where z = L.weightedDiff ws doubleDiff xs ys

prop_diff_symmetric :: [Double] -> [Double] -> Bool
prop_diff_symmetric xs ys = L.diff doubleDiff xs ys == L.diff doubleDiff ys xs

prop_weightedDiff_symmetric :: TestWeights -> [Double] -> [Double] -> Bool
prop_weightedDiff_symmetric (TestWeights ws) xs ys
  = L.weightedDiff ws doubleDiff xs ys == L.weightedDiff ws doubleDiff ys xs

prop_zero_adjustment_is_no_adjustment :: TestData -> Bool
prop_zero_adjustment_is_no_adjustment (TestData _ xs ys)
  = L.makeSimilar makeOrdFractionalSimilar xs 0 ys == ys

prop_full_adjustment_gives_perfect_match :: TestData -> Bool
prop_full_adjustment_gives_perfect_match (TestData _ xs ys)
  = Q.within 20 d 0
  where ys' = L.makeSimilar makeOrdFractionalSimilar xs 1 ys
        d = L.diff doubleDiff xs ys'

prop_makeSimilar_improves_similarity
  :: TestData -> UnitInterval -> Property
prop_makeSimilar_improves_similarity
  (TestData _ xs ys) (FromDouble r) =
    xs /= ys ==> d2 < d1
      where d1 = L.diff doubleDiff xs ys
            d2 = L.diff doubleDiff xs ys'
            ys' = L.makeSimilar makeOrdFractionalSimilar xs r ys

prop_makeSimilar_doesnt_choke_on_infinite_first_list ::
  [Double] -> UnitInterval -> Bool
prop_makeSimilar_doesnt_choke_on_infinite_first_list xs (FromDouble d)
  = length (L.makeSimilar makeOrdFractionalSimilar xs d [0,1..]) == length xs

prop_makeSimilar_doesnt_choke_on_infinite_second_list ::
  [Double] -> UnitInterval -> Bool
prop_makeSimilar_doesnt_choke_on_infinite_second_list xs (FromDouble d)
  = length (L.makeSimilar makeOrdFractionalSimilar [0,1..] d xs) == length xs

test :: Test
test = testGroup "Data.Datamining.Pattern.ListQC"
  [
    testProperty "prop_diff_can_be_0"
      prop_diff_can_be_0,
    testProperty "prop_weightedDiff_can_be_0"
      prop_weightedDiff_can_be_0,
    testProperty "prop_diff_can_be_1"
      prop_diff_can_be_1,
    testProperty "prop_weightedDiff_can_be_1"
      prop_weightedDiff_can_be_1,
    testProperty "prop_diff_btw_0_and_1"
      prop_diff_btw_0_and_1,
    testProperty "prop_weightedDiff_btw_0_and_1"
      prop_weightedDiff_btw_0_and_1,
    testProperty "prop_diff_symmetric"
      prop_diff_symmetric,
    testProperty "prop_weightedDiff_symmetric"
      prop_weightedDiff_symmetric,
    testProperty "prop_zero_adjustment_is_no_adjustment"
      prop_zero_adjustment_is_no_adjustment,
    testProperty "prop_full_adjustment_gives_perfect_match"
      prop_full_adjustment_gives_perfect_match,
    testProperty "prop_makeSimilar_improves_similarity"
      prop_makeSimilar_improves_similarity,
    testProperty "prop_makeSimilar_doesnt_choke_on_infinite_first_list"
      prop_makeSimilar_doesnt_choke_on_infinite_first_list,
    testProperty "prop_makeSimilar_doesnt_choke_on_infinite_second_list"
      prop_makeSimilar_doesnt_choke_on_infinite_second_list
  ]
