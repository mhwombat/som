{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-

This program builds a Self-Organising Map (SOM) based on the colours in an
image. As the SOM learns the colours in the image, it builds a map of the
colour distribution of the image. The map here is a hexagonal grid with 7
tiles in it, so it divides the colours into 7 clusters. The value associated
with each tile is the RGB value of the colour for that cluster.

The picture at <https://github.com/mhwombat/som/blob/master/examples/somTutorial.png?raw=true> 
will make this clearer.
-}

import Data.Datamining.Clustering.SOM (adjustVector, euclideanDistanceSquared,
  gaussian, normalise, Pattern(..), trainBatch)
import Data.Ix (Ix)
import Codec.Image.DevIL (ilInit, readImage, writeImage)
import Control.Monad (forM_)
import Control.Monad.Random (evalRandIO, Rand, RandomGen, getRandomRs)
import Data.List (foldl')
import Data.Word (Word8)
import Data.Array.IArray (elems)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (runSTArray)
import GHC.Arr (listArray, readSTArray, thawSTArray, writeSTArray)
import Math.Geometry.Grid (Grid, HexHexGrid, hexHexGrid)
import qualified Math.Geometry.GridMap as GM (lazyGridMap, map, GridMap, 
  elems, toList)
import Numeric (showHex)

main = do
  -- Initialise the image library
  ilInit
  -- Read the image
  img <- readImage "SampleTiny.png"
  -- Convert the image data to vectors and shuffle them. (If we didn't shuffle
  -- them, the colours at the bottom right of the image would take precedence
  -- over those at the top left.)
  xs <- evalRandIO $ shuffle $ img2vectors img
  -- Build a self-organising map (SOM) initialised with small random values.
  c <- evalRandIO buildSOM
  -- Train it with the vectors from the image.
  let c2 = trainBatch (gaussian 1.0 1.0) c xs
  let c3 = trainBatch (gaussian 0.9 0.9) c2 xs
  let c4 = trainBatch (gaussian 0.8 0.8) c3 xs
  let c5 = trainBatch (gaussian 0.7 0.7) c3 xs
  let c6 = trainBatch (gaussian 0.6 0.6) c3 xs
  let c7 = trainBatch (gaussian 0.5 0.5) c3 xs
  let c8 = trainBatch (gaussian 0.4 0.4) c3 xs
  let c9 = trainBatch (gaussian 0.3 0.3) c3 xs
  let c10 = trainBatch (gaussian 0.2 0.2) c3 xs
  -- Save the SOM as an image.
  print $ GM.toList $ GM.map vector2hex c10

---- Build a self-organising map (SOM) initialised with random values.
buildSOM :: RandomGen r => Rand r (GM.GridMap HexHexGrid (Int,Int) Pixel)
buildSOM = do
  ps <- sequence $ replicate 7 emptyPattern
  return $ GM.lazyGridMap (hexHexGrid 2) ps

vector2hex :: [Double] -> String
vector2hex xs = '#' : foldr (showHex . round) "" xs

-- | Shuffle a list. From <http://www.haskell.org/haskellwiki/Random_shuffle>
shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- take l `fmap` getRandomRs (0, l-1)
  let ar = runSTArray $ do
      ar' <- thawSTArray $ listArray (0, l-1) xs
      forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
          vi <- readSTArray ar' i
          vj <- readSTArray ar' j
          writeSTArray ar' j vi
          writeSTArray ar' i vj
      return ar'
  return (elems ar)

-- We use Doubles instead of Word8s to represent the red, green, blue and 
-- alpha components of a pixel because we're goiing to multiply them by 
-- fractions.
type Pixel = [Double]

-- Creates an "empty" pattern (i.e., one that consists of random values)
-- of length n.
emptyPattern :: RandomGen r => Rand r Pixel
emptyPattern = do
  xs <- getRandomRs (0.0, 255.0)
  return (take 3 xs)

--
-- The code below converts the image data into a sequence of vectors. There's
-- a vector for each pixel in the image. Each vector represents one pixel,
-- and consists of three numbers between 0 and 255, for the red, green, 
-- and blue components of that pixel. (We're omitting the alpha component.)
--

instance Pattern Pixel Double where
   difference = euclideanDistanceSquared
   makeSimilar = adjustVector

img2vectors :: UArray (Int, Int, Int) Word8 -> [Pixel]
img2vectors img = map (take 3) $ chunksOf 4 $ map fromIntegral $ elems img

--vectors2img :: [Pixel] -> UArray (Int, Int, Int) Word8
--vectors2img ps = array (a,b) 
--  where ps' = concatMap f ps
--        f (Pixel xs) = map round xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = fst $ takeNextChunk n ([],xs)

takeNextChunk :: Int -> ([[a1]], [a1]) -> ([[a1]], [a])
takeNextChunk _ (as,[]) = (as,[])
takeNextChunk n (as,xs) = takeNextChunk n (a:as,xs')
  where (a,xs') = splitAt n xs



