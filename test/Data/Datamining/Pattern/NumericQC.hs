------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.NumericQC
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module Data.Datamining.Pattern.NumericQC
  (
    test,
    UnitInterval(FromDouble)
  ) where

import           Data.Datamining.Pattern.Numeric
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

newtype UnitInterval = FromDouble Double deriving Show

instance Arbitrary UnitInterval where
  arbitrary = FromDouble <$> choose (0,1)

prop_diff_can_be_0 :: Double -> Bool
prop_diff_can_be_0 x = diff x x == 0

prop_diff_can_be_1 :: Bool
prop_diff_can_be_1 = diff minDouble maxDouble == 1

prop_diff_btw_0_and_1 :: Double -> Double -> Bool
prop_diff_btw_0_and_1 x y = 0 <= z && z <= 1
  where z = diff x y

prop_diff_symmetric :: Double -> Double -> Bool
prop_diff_symmetric x y = diff x y == diff y x

prop_zero_adjustment_is_no_adjustment ::  Double -> Double -> Bool
prop_zero_adjustment_is_no_adjustment a b = diff b b' == 0
  where b' = makeSimilar a 0 b

prop_full_adjustment_gives_perfect_match ::  Double -> Double -> Bool
prop_full_adjustment_gives_perfect_match a b = diff b' a < aTad
  where b' = makeSimilar a 1 b
        aTad = 1e-10

prop_makeSimilar_improves_similarity ::
  Double -> Double -> UnitInterval -> Property
prop_makeSimilar_improves_similarity a b (FromDouble r)
  = r > 0 && a /= b ==> d2 < d1
      where d1 = diff a b
            d2 = diff a b'
            b' = makeSimilar a r b

test :: Test
test = testGroup "Data.Datamining.Pattern.NumericQC"
  [
    testProperty "prop_diff_can_be_0"
      prop_diff_can_be_0,
    testProperty "prop_diff_can_be_1"
      prop_diff_can_be_1,
    testProperty "prop_diff_btw_0_and_1"
      prop_diff_btw_0_and_1,
    testProperty "prop_diff_symmetric"
      prop_diff_symmetric,
    testProperty "prop_zero_adjustment_is_no_adjustment"
      prop_zero_adjustment_is_no_adjustment,
    testProperty "prop_full_adjustment_gives_perfect_match"
      prop_full_adjustment_gives_perfect_match,
    testProperty "prop_makeSimilar_improves_similarity"
      prop_makeSimilar_improves_similarity
  ]
