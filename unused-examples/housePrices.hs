{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

{-

This program builds a Self-Organising Map (SOM) based on some (fake)
housing prices. As the SOM learns the data, it builds a map of the
data. The map here is a square grid with 4 tiles in it, so it divides
the prices into 4 clusters. The value associated with each tile is the
model ("typical" price) of the houses in that cluster.
-}

import Control.Monad (foldM_, forM_, unless, replicateM)
import Control.Monad.Random (evalRandIO, Rand, RandomGen, getRandomR)
import Data.Datamining.Pattern (adjustVector,
  euclideanDistanceSquared)
import Data.Datamining.Clustering.SOM (SOM(..), toGridMap, decayingGaussian)
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
  putStr . show . map round . GM.elems . toGridMap $ c
  foldM_ trainAndPrint c inputData

trainAndPrint
  :: SOM Double Double (LGridMap RectSquareGrid) Double (Int, Int) Price ->
     Price -> IO (SOM Double Double (LGridMap RectSquareGrid) Double (Int, Int) Price)
trainAndPrint c x = do
  let c2 = train c x
  putStr . show . map round . GM.elems . toGridMap $ c2
  putStrLn $ " after training with " ++ show (round x)
  return c2

-- Build a classifier initialised with random values.
-- I used random values here just to emphasise that the map organises
-- itself. In practice, however, you could choose initial values that
-- are spread evenly over the expected range of input values.
buildSOM
  :: RandomGen r
    => Int -> Rand r (SOM Double Double (LGridMap RectSquareGrid) Double (Int, Int) Price)
buildSOM n = do
  let g = rectSquareGrid 1 4  -- The grid we'll use for our SOM
  ps <- replicateM (tileCount g) randomPrice -- random initial values
  let gm = lazyGridMap g ps -- a map from grid positions to prices
  let n' = fromIntegral n
  let lrf = decayingGaussian 0.5 0.1 0.3 0.1 n' -- learning rate function
  return $ SOM gm lrf absDifference adjustNum 0

randomPrice :: RandomGen r => Rand r Price
randomPrice = getRandomR (0, 500000)

type Price = Double

absDifference :: Price -> Price -> Double
absDifference x y = abs (x - y)

adjustNum :: Price -> Double -> Price -> Price
adjustNum target r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = x + r*(target - x)

-- Here's the data.
-- It includes:
--    80 numbers from a Gaussian distribution with mean 75000
--   200 numbers from a Gaussian distribution with mean 150000
--   150 numbers from a Gaussian distribution with mean 350000
--    40 numbers from a Gaussian distribution with mean 900000
-- In each case, the standard deviation is 1000.
-- The numbers have been shuffled.
inputData :: [Price]
inputData = [72160,151100,351600,350100,347700,149500,349500,73100,
  350400,350700,73770,350500,348700,349900,75690,350300,350300,348700,
  349600,76040,75510,350100,150200,150300,77440,150900,152600,350500,
  149300,349800,348200,149500,349000,350400,150000,349800,352000,347800,
  149700,349100,347700,150300,350900,899400,73920,150700,349300,149600,
  150000,349300,350600,351000,350900,351200,75250,349000,898300,351300,
  350700,150000,351000,148800,348900,149900,149900,348900,75390,151600,
  349300,149300,149400,900500,349700,351400,149400,149600,75830,351000,
  150300,147700,350000,899100,899700,150600,150200,148600,76890,148800,
  350500,898200,350300,351100,898900,148300,150200,149700,151000,351500,
  150000,896700,350900,151100,150000,74040,150700,148500,351700,901600,
  151400,148500,148400,150800,899100,150200,150200,898000,349100,74220,
  150800,350700,349100,149500,150800,150100,149400,74260,150800,74260,
  901000,74820,75200,74300,349400,350900,349200,351300,75250,348900,
  150100,151300,150200,900700,351000,150000,152000,151800,75310,899200,
  149200,150800,150700,150000,72880,149400,150000,350500,74130,151400,
  898700,149600,149300,898300,150900,151300,150500,349300,348700,147800,
  75770,150800,150200,148700,351000,152300,350600,150900,77520,149400,
  149600,75980,350200,348900,150100,148700,900700,899600,151000,75470,
  899700,75290,349900,349000,148700,150700,350100,148900,350300,150600,
  899500,150600,350000,74310,149400,898500,150900,900700,149800,350400,
  149600,149300,151600,75030,74910,148800,150400,350500,348600,350700,
  75580,74510,149400,150300,150400,350700,78820,149800,349300,349700,
  350700,899400,151400,76370,349600,76500,350700,149400,151300,349300,
  75990,151000,347600,350800,349100,149100,149700,75280,350300,149700,
  151000,899700,352000,349800,150100,351400,349800,150500,348200,148900,
  349900,349900,75360,148500,149400,150300,149200,76120,151300,151400,
  149500,898500,150800,74380,151200,75630,149500,150900,350600,148800,
  150000,74600,150700,151700,150600,348600,149500,75510,349400,350600,
  149600,150200,350800,77250,74790,150500,350100,900900,900300,150600,
  149800,899700,151100,349800,75510,74670,349800,150200,348800,73940,
  151400,350200,900200,151500,150200,76830,350600,75760,75570,350200,
  149500,350100,150600,74030,351200,150700,150100,149600,74660,350500,
  73600,350800,150200,350600,349200,350400,349900,149800,350000,349300,
  74320,148900,900300,151400,74640,75940,149900,349400,149600,350400,
  899800,349300,149100,900400,151000,150000,350600,76730,76000,150300,
  73610,349300,149300,150700,347500,899900,149000,75810,151500,349100,
  351000,74820,74600,350700,349100,74050,75050,349900,352200,149900,
  350600,73340,901000,75280,350300,149600,148700,350300,150000,74350,
  900600,150300,153000,150800,350100,350300,148200,152400,349200,150400,
  148700,148100,900000,150300,351700,149500,901400,73440,73450,75420,
  150000,349400,150600,149800,349000,149900,149400,349900,148800,151100,
  900200,349600,149900,151200,351300,350400,75460,76030,149500,74270,
  349000,348500,150300,149100,898000,73560,150600,150500,149500,152700,
  349400,146900,148700,349300,350000,149600,150400,148200,73960,350700,
  149400,148300,151000,350400,348900,150800,75120,149800,348100,75860,
  74140,150300,351300,150300,150000,899800,349900,151500,350100,151500,
  150200,149500]
