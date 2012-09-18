{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Data.Datamining.Clustering.SOMQC ( test )

import Test.Framework as TF ( defaultMain, Test )

tests ∷ [TF.Test]
tests = 
  [ 
    Data.Datamining.Clustering.SOMQC.test
  ]

main ∷ IO ()
main = defaultMain tests
