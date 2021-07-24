------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.PatternQC
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}

module Data.Datamining.PatternQC
  (
    test
  ) where

import           Data.Datamining.Pattern

import           Test.Framework                       as TF (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary, Gen, Property,
                                                       arbitrary, choose,
                                                       property, sized, vector,
                                                       (==>))

#if MIN_VERSION_base(4,8,0)
#else
import           Control.Applicative
#endif

newtype UnitInterval = FromDouble Double deriving Show

instance Arbitrary UnitInterval where
  arbitrary = FromDouble <$> choose (0,1)

prop_adjustVector_doesnt_choke_on_infinite_lists ::
  [Double] -> UnitInterval -> Property
prop_adjustVector_doesnt_choke_on_infinite_lists xs (FromDouble d) =
  property $
    length (adjustVector xs d [0,1..]) == length xs

data TwoVectorsSameLength = TwoVectorsSameLength [Double] [Double]
  deriving Show

sizedTwoVectorsSameLength :: Int -> Gen TwoVectorsSameLength
sizedTwoVectorsSameLength n =
  TwoVectorsSameLength <$> vector n <*> vector n

instance Arbitrary TwoVectorsSameLength where
  arbitrary = sized sizedTwoVectorsSameLength

prop_zero_adjustment_is_no_adjustment ::
  TwoVectorsSameLength -> Property
prop_zero_adjustment_is_no_adjustment (TwoVectorsSameLength xs ys) =
  property $ adjustVector xs 0 ys == ys

prop_full_adjustment_gives_perfect_match ::
  TwoVectorsSameLength -> Property
prop_full_adjustment_gives_perfect_match (TwoVectorsSameLength xs ys) =
  property $ adjustVector xs 1 ys == xs

prop_adjustVector_improves_similarity ::
  TwoVectorsSameLength -> UnitInterval -> Property
prop_adjustVector_improves_similarity
  (TwoVectorsSameLength xs ys) (FromDouble a) =
    a > 0 && a < 1 && not (null xs) ==> d2 < d1
      where d1 = euclideanDistanceSquared xs ys
            d2 = euclideanDistanceSquared xs ys'
            ys' = adjustVector xs a ys

test :: Test
test = testGroup "QuickCheck Data.Datamining.Clustering.PatternQC"
  [
    testProperty "prop_adjustVector_doesnt_choke_on_infinite_lists"
      prop_adjustVector_doesnt_choke_on_infinite_lists,
    testProperty "prop_zero_adjustment_is_no_adjustment"
      prop_zero_adjustment_is_no_adjustment,
    testProperty "prop_full_adjustment_gives_perfect_match"
      prop_full_adjustment_gives_perfect_match,
    testProperty "prop_adjustVector_improves_similarity"
      prop_adjustVector_improves_similarity
  ]

