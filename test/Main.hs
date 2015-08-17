------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Data.Datamining.PatternQC ( test )
import Data.Datamining.Clustering.SOMQC ( test )
import Data.Datamining.Clustering.SOSQC ( test )
import Data.Datamining.Clustering.SSOMQC ( test )
import Data.Datamining.Clustering.DSOMQC ( test )

import Test.Framework as TF ( defaultMain, Test )

tests :: [TF.Test]
tests = 
  [ 
    Data.Datamining.PatternQC.test,
    Data.Datamining.Clustering.SSOMQC.test,
    Data.Datamining.Clustering.SOSQC.test,
    Data.Datamining.Clustering.SOMQC.test,
    Data.Datamining.Clustering.DSOMQC.test
  ]

main :: IO ()
main = defaultMain tests
