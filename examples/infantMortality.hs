{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

{-

This program builds a Self-Organising Map (SOM) based on EU mortality
rates for 2011. As the SOM learns the rates in the image, it builds a 
map of the data. The map here is a rectangular grid with 3
tiles in it, so it divides the values into 7 clusters. The value 
associated with each tile is the model (i.e., the "typical" death rate)
for that cluster.
-}

import Codec.Image.DevIL (ilInit, readImage)
import Control.Monad (foldM_, forM_, unless, replicateM)
import Control.Monad.Random (evalRandIO, Rand, RandomGen, getRandomR)
import Data.Datamining.Pattern (adjustVector,
  euclideanDistanceSquared, Pattern(..))
import Data.Datamining.Clustering.SOM (SOM(..), defaultSOM, toGridMap)
import Data.Datamining.Clustering.Classifier (Classifier, train, trainBatch)
import Data.List (foldl')
import Data.Word (Word8)
import Data.Array.IArray (elems)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (runSTArray)
import GHC.Arr (listArray, readSTArray, thawSTArray, writeSTArray)
import Math.Geometry.Grid (Index, tileCount)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import qualified Math.Geometry.GridMap as GM (GridMap, BaseGrid, elems,
  map, toList)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import Numeric (showHex)
import System.Directory (doesFileExist)


main :: IO ()
main = do
  -- Build a self-organising map (SOM) initialised with small random values.
  c <- evalRandIO $ buildSOM (length inputData)
  putStrLn $ show c ++ " initial (random) patterns"
  foldM_ trainAndPrint c inputData

trainAndPrint :: SOM (LGridMap RectSquareGrid) (Int, Int) Deaths -> Deaths -> IO (SOM (LGridMap RectSquareGrid) (Int, Int) Deaths)
trainAndPrint c x = do
  let c2 = train c x
  putStrLn $ show c2 ++ " after training with " ++ show x
  return c2

-- Build a classifier initialised with random values.
buildSOM :: RandomGen r => Int -> Rand r (SOM (LGridMap RectSquareGrid) k Deaths)
buildSOM n = do
  let g = rectSquareGrid 1 3  -- The grid we'll use for our SOM
  ps <- replicateM (tileCount g) randomDeaths -- random initial values
  let gm = lazyGridMap g ps -- a map from grid positions to prices
  return $ defaultSOM gm 1.0 0.3 n

randomDeaths :: RandomGen r => Rand r Deaths
randomDeaths = getRandomR (0, 10)

instance Pattern Deaths where
  type Metric Deaths = Double
  difference x y = abs (x - y)
  makeSimilar x amount y = y + amount*(x - y)

instance Show (SOM (LGridMap RectSquareGrid) (Int, Int) Deaths) where
 show c = show . GM.elems . toGridMap $ c

type Deaths = Double

-- Here's the data we will analyse:
-- Deaths per 1 000 live births, per country, in 2011
-- Source: Eurostat (online data code: demo_minfind)
-- http://epp.eurostat.ec.europa.eu/statistics_explained/index.php/Mortality_and_life_expectancy_statistics
--
-- Notes:
-- (1) 2010 instead of 2011.
-- (2) Excluding French overseas departments in 1996.
-- (3) 2011, provisional data.
inputData :: [Deaths]
inputData = [
  4.1, -- EU-27 (1)(2)
  3.3, -- Belgium (3)
  8.5, -- Bulgaria
  2.7, -- Czech Republic
  3.5, -- Denmark
  3.6, -- Germany (3)
  2.5, -- Estonia
  3.5, -- Ireland
  3.7, -- Greece (3)
  3.2, -- Spain (3)
  3.6, -- France (1)(2)
  3.2, -- Italy
  3.1, -- Cyprus
  6.6, -- Latvia
  4.2, -- Lithuania
  4.3, -- Luxembourg
  4.9, -- Hungary (3)
  6.1, -- Malta
  3.6, -- Netherlands
  3.6, -- Austria
  4.7, -- Poland
  3.1, -- Portugal
  9.4, -- Romania
  2.9, -- Slovenia (3)
  4.9, -- Slovakia
  2.4, -- Finland
  2.1, -- Sweden
  4.3, -- United Kingdom (1)
  0.9, -- Iceland
  2.5, -- Liechtenstein (3)
  2.4, -- Norway
  3.8, -- Switzerland (3)
  6.7, -- Montenegro (1)
  4.7, -- Croatia
  7.6, -- FYR of Macedonia (3)
  12.6 -- Turkey (3)
  ]
