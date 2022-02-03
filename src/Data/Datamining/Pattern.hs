------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.Numeric
-- Copyright   :  (c) 2017-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with patterns.
--
------------------------------------------------------------------------

module Data.Datamining.Pattern
  (
    prop_diff_can_be_0,
    prop_diff_can_be_1,
    prop_diff_btw_0_and_1,
    prop_diff_symmetric,
    prop_makeSimilar_can_leave_unchanged,
    prop_makeSimilar_can_match_perfectly,
    prop_makeSimilar_improves_similarity,
    prop_makeSimilar_improves_integral_similarity
  ) where

import           Test.QuickCheck (Property, (==>))

--
-- Utilities for testing functions that measure the difference
-- between two patterns.
--

prop_diff_can_be_0 :: (Num b, Eq b) => (a -> a -> b) -> a -> Bool
prop_diff_can_be_0 f x = f x x == 0

prop_diff_can_be_1 :: (Num b, Eq b) => (a -> a -> b) -> a -> a -> Bool
prop_diff_can_be_1 f extreme1 extreme2 = f extreme1 extreme2 == 1

prop_diff_btw_0_and_1 :: (Num b, Ord b) => (a -> a -> b) -> a -> a -> Bool
prop_diff_btw_0_and_1 f x y = 0 <= d && d <= 1
  where d = f x y

prop_diff_symmetric :: Eq b => (a -> a -> b) -> a -> a -> Bool
prop_diff_symmetric f x y = f x y == f y x

--
-- Utilities for testing functions that adjust one pattern to make it
-- more similar to another.
--
prop_makeSimilar_can_leave_unchanged
  :: (Eq a, Num b) => (a -> b -> a -> a) -> (a -> a -> Bool) -> a -> a -> Bool
prop_makeSimilar_can_leave_unchanged makeSimilar equiv t x
  = x' `equiv` x
  where x' = makeSimilar t 0 x

prop_makeSimilar_can_match_perfectly
  :: (Eq a, Num b) => (a -> b -> a -> a) -> (a -> a -> Bool) -> a -> a -> Bool
prop_makeSimilar_can_match_perfectly makeSimilar equiv t x
  = x' `equiv` t
  where x' = makeSimilar t 1 x

prop_makeSimilar_improves_similarity
  :: (Num a, Ord a, Num b, Ord b, Ord c)
  => (a -> b -> a -> a) -> (a -> a -> c) -> a -> b -> a -> Property
prop_makeSimilar_improves_similarity makeSimilar diff t r x
  = r > 0 && r < 1 && t /= x ==> diff t x' < diff t x
  where x' = makeSimilar t r x

prop_makeSimilar_improves_integral_similarity
  :: (Eq a, Num a, Integral a, Ord b, RealFrac b)
  => (a -> b -> a -> a) -> a -> b -> a -> Property
prop_makeSimilar_improves_integral_similarity f t r x
  = t /= x && r' > rMin ==> d2 < d1
  where d1 = abs (fromIntegral t - fromIntegral x) :: Double
        d2 = abs (fromIntegral t - fromIntegral x') :: Double
        x' = f t r x
        r' = realToFrac r :: Double
        rMin = abs $ 0.5 / (fromIntegral t - fromIntegral x) :: Double

