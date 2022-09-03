------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.NumericQC
-- Copyright   :  (c) 2017-2022 Amy de Buitléir
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

import Data.Datamining.Pattern
import Data.Datamining.Pattern.Numeric
import Data.Word                            (Word8)
import System.Random                        (Random)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Counterexamples

newtype UnitInterval = FromDouble Double
  deriving (Read, Show, Eq, Num, Real, Fractional, RealFrac, Ord)

instance Arbitrary UnitInterval where
  arbitrary = FromDouble <$> choose (0,1)

newtype BDouble = BDouble Double
  deriving (Read, Show, Num, Ord, Eq, Real, Fractional, Floating, Random, RealFrac, RealFloat)

instance Bounded BDouble where
  minBound = 7
  maxBound = 9

instance Arbitrary BDouble where
  arbitrary = choose (minBound, maxBound)

approxEq :: (Ord a, Fractional a) => a -> a -> Bool
approxEq x y = absDifference x y < 1e-10

test :: Test
test = testGroup "Data.Datamining.Pattern.NumericQC"
  [
    testProperty "prop_boundedFractionalDiff_can_be_0"
      (prop_diff_can_be_0 (boundedFractionalDiff :: BDouble -> BDouble -> Double)),
    testProperty "prop_boundedFractionalDiff_can_be_1"
      (prop_diff_can_be_1 (boundedFractionalDiff :: BDouble -> BDouble -> Double)
        minBound maxBound),
    testProperty "prop_boundedFractionalDiff_btw_0_and_1"
      (prop_diff_btw_0_and_1 (boundedFractionalDiff :: BDouble -> BDouble -> Double)),
    testProperty "prop_boundedFractionalDiff_symmetric"
      (prop_diff_symmetric (boundedFractionalDiff :: BDouble -> BDouble -> Double)),

    -- testProperty "prop_realFloatDiff_can_be_0"
    --   (prop_diff_can_be_0 (realFloatDiff :: Double -> Double -> Double)),
    -- testProperty "prop_realFloatDiff_can_be_1"
    --   (prop_diff_can_be_1 (realFloatDiff :: Double -> Double -> Double)
    --     minDouble maxDouble),
    -- testProperty "prop_realFloatDiff_btw_0_and_1"
    --   (prop_diff_btw_0_and_1 (realFloatDiff :: Double -> Double -> Double)),
    -- testProperty "prop_realFloatDiff_symmetric"
    --   (prop_diff_symmetric (realFloatDiff :: Double -> Double -> Double)),

    testProperty "prop_doubleDiff_can_be_0"
      (prop_diff_can_be_0 (doubleDiff :: Double -> Double -> Double)),
    testProperty "prop_doubleDiff_can_be_1"
      (prop_diff_can_be_1 (doubleDiff :: Double -> Double -> Double)
        minDouble maxDouble),
    testProperty "prop_doubleDiff_btw_0_and_1"
      (prop_diff_btw_0_and_1 (doubleDiff :: Double -> Double -> Double)),
    testProperty "prop_doubleDiff_symmetric"
      (prop_diff_symmetric (doubleDiff :: Double -> Double -> Double)),

    testProperty "prop_boundedIntegralDiff_can_be_0"
      (prop_diff_can_be_0 (boundedIntegralDiff :: Word8 -> Word8 -> Double)),
    testProperty "prop_boundedIntegralDiff_can_be_1"
      (prop_diff_can_be_1 (boundedIntegralDiff :: Word8 -> Word8 -> Double)
        minBound maxBound),
    testProperty "prop_boundedIntegralDiff_btw_0_and_1"
      (prop_diff_btw_0_and_1 (boundedIntegralDiff :: Word8 -> Word8 -> Double)),
    testProperty "prop_boundedIntegralDiff_symmetric"
      (prop_diff_symmetric (boundedIntegralDiff :: Word8 -> Word8 -> Double)),

    testProperty "prop_makeRealFracSimilar_can_leave_unchanged"
      (prop_makeSimilar_can_leave_unchanged
        (makeRealFracSimilar :: Float -> UnitInterval -> Float -> Float)
        (==)),
    testProperty "prop_makeRealFracSimilar_can_match_perfectly"
      (prop_makeSimilar_can_match_perfectly
        (makeRealFracSimilar :: Float -> UnitInterval -> Float -> Float)
        (==)),
    testProperty "prop_makeRealFracSimilar_improves_similarity"
      (prop_makeSimilar_improves_similarity
        (makeRealFracSimilar :: Float -> UnitInterval -> Float -> Float)
        absDifference),

    testProperty "prop_makeOrdFractionalSimilar_can_leave_unchanged"
      (prop_makeSimilar_can_leave_unchanged
        (makeOrdFractionalSimilar :: Double -> Double -> Double -> Double)
        (==)),
    testProperty "prop_makeOrdFractionalSimilar_can_match_perfectly"
      (prop_makeSimilar_can_match_perfectly
        (makeOrdFractionalSimilar :: Double -> Double -> Double -> Double)
        approxEq),
    testProperty "prop_makeOrdFractionalSimilar_improves_similarity"
      (prop_makeSimilar_improves_similarity
        (makeOrdFractionalSimilar :: Double -> Double -> Double -> Double)
        absDifference),

    testProperty "prop_makeIntegralSimilar_can_leave_unchanged"
      (prop_makeSimilar_can_leave_unchanged
        (makeIntegralSimilar :: Word8 -> UnitInterval -> Word8 -> Word8)
        (==)),
    testProperty "prop_makeIntegralSimilar_can_match_perfectly"
      (prop_makeSimilar_can_match_perfectly
        (makeIntegralSimilar :: Word8 -> UnitInterval -> Word8 -> Word8)
        (==)),
    testProperty "prop_makeIntegralSimilar_improves_similarity"
      (prop_makeSimilar_improves_integral_similarity
        (makeIntegralSimilar :: Word8 -> UnitInterval -> Word8 -> Word8))
  ]
