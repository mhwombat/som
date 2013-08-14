{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Data.Datamining.PatternQC ( test )
import Data.Datamining.Clustering.SOMQC ( test )
import Data.Datamining.Clustering.DSOMQC ( test )

import Test.Framework as TF ( defaultMain, Test )

tests :: [TF.Test]
tests = 
  [ 
    Data.Datamining.PatternQC.test,
    Data.Datamining.Clustering.SOMQC.test,
    Data.Datamining.Clustering.DSOMQC.test
  ]

main :: IO ()
main = defaultMain tests
