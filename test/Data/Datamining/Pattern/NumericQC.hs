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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Datamining.Pattern.NumericQC
  (
    test,
    UnitInterval(FromDouble)
  ) where

import           Data.Datamining.Pattern.Numeric
import           Data.Word                            (Word8)
import           System.Random                        (Random)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck.Counterexamples

newtype UnitInterval = FromDouble Double
  deriving (Read, Show, Eq, Num, Real, Ord)

instance Arbitrary UnitInterval where
  arbitrary = FromDouble <$> choose (0,1)

newtype BDouble = BDouble Double
  deriving (Read, Show, Num, Ord, Eq, Real, Fractional, Random)

instance Bounded BDouble where
  minBound = 7
  maxBound = 9

instance Arbitrary BDouble where
  arbitrary = choose (minBound, maxBound)

-- TODO make the tests polymorphic, and test with different types.

prop_boundedFractionalDiff_can_be_0 :: BDouble -> Bool
prop_boundedFractionalDiff_can_be_0 x = d == 0
  where d = boundedFractionalDiff x x :: Double

prop_boundedFractionalDiff_can_be_1 :: Bool
prop_boundedFractionalDiff_can_be_1 = d == 1
  where d = boundedFractionalDiff x y :: Double
        x = minBound :: BDouble
        y = maxBound :: BDouble

prop_boundedFractionalDiff_btw_0_and_1 :: BDouble -> BDouble -> Bool
prop_boundedFractionalDiff_btw_0_and_1 x y = 0 <= d && d <= 1
  where d = boundedFractionalDiff x y :: Double

prop_boundedFractionalDiff_symmetric :: BDouble -> BDouble -> Bool
prop_boundedFractionalDiff_symmetric x y = d1 == d2
  where d1 = boundedFractionalDiff x y :: Double
        d2 = boundedFractionalDiff y x :: Double


prop_makeRealFracSimilar_can_leave_unchanged ::  Float -> Float -> Bool
prop_makeRealFracSimilar_can_leave_unchanged t x = x' == x
  where x' = makeRealFracSimilar t (0 :: Double) x

prop_makeRealFracSimilar_can_match_perfectly ::  Float -> Float -> Bool
prop_makeRealFracSimilar_can_match_perfectly t x = x' == t
  where x' = makeRealFracSimilar t (1 :: Double) x

prop_makeRealFracSimilar_improves_similarity ::
  Float -> Float -> UnitInterval -> Property
prop_makeRealFracSimilar_improves_similarity t x (FromDouble r)
  = r > 0 && t /= x ==> d2 < d1
      where d1 = abs (t - x)
            d2 = abs (t - x')
            x' = makeRealFracSimilar t r x


prop_boundedIntegralDiff_can_be_0 :: Word8 -> Bool
prop_boundedIntegralDiff_can_be_0 x = d == 0
  where d = boundedIntegralDiff x x :: Double

prop_boundedIntegralDiff_can_be_1 :: Bool
prop_boundedIntegralDiff_can_be_1 = d == 1
  where d = boundedIntegralDiff x y :: Double
        x = minBound :: Word8
        y = maxBound :: Word8

prop_boundedIntegralDiff_btw_0_and_1 :: Word8 -> Word8 -> Bool
prop_boundedIntegralDiff_btw_0_and_1 x y = 0 <= d && d <= 1
  where d = boundedIntegralDiff x y :: Double

prop_boundedIntegralDiff_symmetric :: Word8 -> Word8 -> Bool
prop_boundedIntegralDiff_symmetric x y = d1 == d2
  where d1 = boundedIntegralDiff x y :: Double
        d2 = boundedIntegralDiff y x :: Double


prop_makeIntegralSimilar_can_leave_unchanged ::  Word8 -> Word8 -> Bool
prop_makeIntegralSimilar_can_leave_unchanged t x = x' == x
  where x' = makeIntegralSimilar t (0 :: Double) x

prop_makeIntegralSimilar_can_match_perfectly ::  Word8 -> Word8 -> Bool
prop_makeIntegralSimilar_can_match_perfectly t x = x' == t
  where x' = makeIntegralSimilar t (1 :: Double) x

prop_makeIntegralSimilar_improves_similarity ::
  Word8 -> Word8 -> UnitInterval -> Property
prop_makeIntegralSimilar_improves_similarity t x (FromDouble r)
  = r > rMin && t /= x ==> d2 < d1
      where d1 = abs (fromIntegral t - fromIntegral x) :: Double
            d2 = abs (fromIntegral t - fromIntegral x') :: Double
            x' = makeIntegralSimilar t r x
            rMin = abs $ 0.5 / d1


prop_realFloatDiff_can_be_0 :: Double -> Bool
prop_realFloatDiff_can_be_0 x = realFloatDiff x x == 0

prop_realFloatDiff_can_be_1 :: Bool
prop_realFloatDiff_can_be_1 = realFloatDiff minDouble maxDouble == 1

prop_realFloatDiff_btw_0_and_1 :: Double -> Double -> Bool
prop_realFloatDiff_btw_0_and_1 x y = 0 <= d && d <= 1
  where d = realFloatDiff x y

prop_realFloatDiff_symmetric :: Double -> Double -> Bool
prop_realFloatDiff_symmetric x y = realFloatDiff x y == realFloatDiff y x


prop_makeOrdFractionalSimilar_can_leave_unchanged ::  Double -> Double -> Bool
prop_makeOrdFractionalSimilar_can_leave_unchanged t x = realFloatDiff x x' == 0
  where x' = makeOrdFractionalSimilar t 0 x

prop_makeOrdFractionalSimilar_can_match_perfectly ::  Double -> Double -> Bool
prop_makeOrdFractionalSimilar_can_match_perfectly t x = d < 1e-10
  where x' = makeOrdFractionalSimilar t 1 x
        d = abs (t - x')

prop_makeOrdFractionalSimilar_improves_similarity ::
  Double -> Double -> UnitInterval -> Property
prop_makeOrdFractionalSimilar_improves_similarity t x (FromDouble r)
  = r > 0 && t /= x ==> d2 < d1
      where d1 = abs (t - x)
            d2 = abs (t - x')
            x' = makeOrdFractionalSimilar t r x

test :: Test
test = testGroup "Data.Datamining.Pattern.NumericQC"
  [
    testProperty "prop_boundedFractionalDiff_can_be_0"
      prop_boundedFractionalDiff_can_be_0,
    testProperty "prop_boundedFractionalDiff_can_be_1"
      prop_boundedFractionalDiff_can_be_1,
    testProperty "prop_boundedFractionalDiff_btw_0_and_1"
      prop_boundedFractionalDiff_btw_0_and_1,
    testProperty "prop_boundedFractionalDiff_symmetric"
      prop_boundedFractionalDiff_symmetric,

    testProperty "prop_makeRealFracSimilar_can_leave_unchanged"
      prop_makeRealFracSimilar_can_leave_unchanged,
    testProperty "prop_makeRealFracSimilar_can_match_perfectly"
      prop_makeRealFracSimilar_can_match_perfectly,
    testProperty "prop_makeRealFracSimilar_improves_similarity"
      prop_makeRealFracSimilar_improves_similarity,

    testProperty "prop_boundedIntegralDiff_can_be_0"
      prop_boundedIntegralDiff_can_be_0,
    testProperty "prop_boundedIntegralDiff_can_be_1"
      prop_boundedIntegralDiff_can_be_1,
    testProperty "prop_boundedIntegralDiff_btw_0_and_1"
      prop_boundedIntegralDiff_btw_0_and_1,
    testProperty "prop_boundedIntegralDiff_symmetric"
      prop_boundedIntegralDiff_symmetric,

    testProperty "prop_makeIntegralSimilar_can_leave_unchanged"
      prop_makeIntegralSimilar_can_leave_unchanged,
    testProperty "prop_makeIntegralSimilar_can_match_perfectly"
      prop_makeIntegralSimilar_can_match_perfectly,
    testProperty "prop_makeIntegralSimilar_improves_similarity"
      prop_makeIntegralSimilar_improves_similarity,

    testProperty "prop_realFloatDiff_can_be_0"
      prop_realFloatDiff_can_be_0,
    testProperty "prop_realFloatDiff_can_be_1"
      prop_realFloatDiff_can_be_1,
    testProperty "prop_realFloatDiff_btw_0_and_1"
      prop_realFloatDiff_btw_0_and_1,
    testProperty "prop_realFloatDiff_symmetric"
      prop_realFloatDiff_symmetric,

    testProperty "prop_makeOrdFractionalSimilar_can_leave_unchanged"
      prop_makeOrdFractionalSimilar_can_leave_unchanged,
    testProperty "prop_makeOrdFractionalSimilar_can_match_perfectly"
      prop_makeOrdFractionalSimilar_can_match_perfectly,
    testProperty "prop_makeOrdFractionalSimilar_improves_similarity"
      prop_makeOrdFractionalSimilar_improves_similarity
  ]
