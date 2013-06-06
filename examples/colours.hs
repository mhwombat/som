{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, TypeFamilies #-}

{-

This program builds a Self-Organising Map (SOM) based on the colours in an
image. As the SOM learns the colours in the image, it builds a map of the
colour distribution of the image. The map here is a hexagonal grid with 7
tiles in it, so it divides the colours into 7 clusters. The value associated
with each tile is the RGB value of the colour for that cluster.

The picture at <https://github.com/mhwombat/som/blob/master/examples/somTutorial.png?raw=true> 
will make this clearer.
-}

import Codec.Image.DevIL (ilInit, readImage)
import Control.Monad (forM_, unless, replicateM)
import Control.Monad.Random (evalRandIO, Rand, RandomGen, getRandomRs)
import Data.Datamining.Pattern (adjustVector, 
  euclideanDistanceSquared, Pattern(..))
import Data.Datamining.Clustering.SOM (SOM, defaultSOM, toGridMap)
import Data.Datamining.Clustering.Classifier (Classifier, train, trainBatch)
import Data.List (foldl')
import Data.Word (Word8)
import Data.Array.IArray (elems)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (runSTArray)
import GHC.Arr (listArray, readSTArray, thawSTArray, writeSTArray)
import Math.Geometry.Grid (Index)
import Math.Geometry.Grid.Hexagonal (HexHexGrid, hexHexGrid)
import qualified Math.Geometry.GridMap as GM (GridMap, BaseGrid, map,
  toList)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import Numeric (showHex)
import System.Directory (doesFileExist)

-- These imports are for the graphics
import Data.Colour.SRGB (sRGB)
import Diagrams.Prelude
import Diagrams.Backend.SVG -- from diagrams-svg
import qualified Data.ByteString.Lazy as BS
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg) -- from blaze-svg
import Text.Blaze.Svg.Internal (Svg) -- from blaze-svg
import Diagrams.TwoD.Text (Text) -- from diagrams-lib

inputFile :: FilePath
inputFile = "Sample.png"

main :: IO ()
main = do
  -- Initialise the image library
  ilInit
  -- Read the image
  xs <- readPixels inputFile
  -- Build a self-organising map (SOM) initialised with small random values.
  c <- evalRandIO $ buildSOM (length xs) :: IO (SOM (LGridMap HexHexGrid) k Pixel)
  -- Train it with the vectors from the image.
  let c2 = foldl' train c xs
  -- Write the result.
  print . GM.toList . GM.map vector2hex . toGridMap $ c2
  -- Create an image file showing the results
  BS.writeFile "map.svg" . renderSvg . drawColourMap . toGridMap $ c2
  putStrLn "The output image is map.svg"

readPixels :: FilePath -> IO [Pixel]
readPixels f = do
  fileExists <- doesFileExist inputFile
  unless fileExists $ error ("Can't find file: " ++ inputFile)
  img <- readImage inputFile
  -- Convert the image data to vectors and shuffle them. (If we didn't shuffle
  -- them, the colours at the bottom right of the image would take precedence
  -- over those at the top left.)
  evalRandIO $ shuffle $ img2vectors img

-- Build a classifier initialised with random values.
buildSOM :: RandomGen r => Int -> Rand r (SOM (LGridMap HexHexGrid) k Pixel)
buildSOM n = do
  ps <- replicateM 7 emptyPattern
  let g = hexHexGrid 2      -- The grid we'll use for our colour map
  let gm = lazyGridMap g ps -- a map from grid positions to colours
  return $ defaultSOM gm 1 0.3 n

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

instance Pattern Pixel where
  type Metric Pixel = Double
  difference = euclideanDistanceSquared
  makeSimilar = adjustVector

--
-- The code below converts the image data into a sequence of vectors. There's
-- a vector for each pixel in the image. Each vector represents one pixel,
-- and consists of three numbers between 0 and 255, for the red, green, 
-- and blue components of that pixel. (We're omitting the alpha component.)
--

img2vectors :: UArray (Int, Int, Int) Word8 -> [Pixel]
img2vectors img = map (take 3) $ chunksOf 4 $ map fromIntegral $ elems img

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = fst $ takeNextChunk n ([],xs)

takeNextChunk :: Int -> ([[a1]], [a1]) -> ([[a1]], [a])
takeNextChunk _ (as,[]) = (as,[])
takeNextChunk n (as,xs) = takeNextChunk n (a:as,xs')
  where (a,xs') = splitAt n xs

--
-- The code below generates the output image
--

drawColourMap :: (GM.GridMap gm Pixel, 
  Index (GM.BaseGrid gm Pixel) ~ (Int, Int)) =>
    gm Pixel -> Svg
drawColourMap c = renderDia SVG (SVGOptions sizeSpec) (pad 1.1 diagram)
  where (centre:outer) = map drawHexagon . GM.toList $ c
        directions = iterate (rotateBy (1/6)) unitX
        ring = zip directions outer
        diagram = appends centre ring

sizeSpec :: SizeSpec2D
sizeSpec = Dims 400.0 400.0

drawHexagon
  :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>
     ((Int, Int), Pixel) -> Diagram b R2
drawHexagon (index, rgb@(r:g:b:_)) = 
  mconcat [ label,
            hexagon 20 # lw 0.02 # fc colour # rotateBy (1/4) ]
  where label = (text (show index) # fc white # fontSize 5 ||| strutY 5)
                ===
                (text (vector2hex rgb) # fc white # fontSize 5 ||| strutY 5)
        colour = sRGB (r/256) (g/256) (b/256)

