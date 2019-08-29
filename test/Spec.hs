------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
import           Data.Datamining.Clustering.DSOMQC (test)
import           Data.Datamining.Clustering.SGM4QC (test)
import           Data.Datamining.Clustering.SGM6QC (test)
import           Data.Datamining.Clustering.SGMQC  (test)
import           Data.Datamining.Clustering.SOMQC  (test)
import           Data.Datamining.PatternQC         (test)

import           Test.Framework                    as TF (Test, defaultMain)

tests :: [TF.Test]
tests =
  [
    Data.Datamining.PatternQC.test,
    Data.Datamining.Clustering.SGMQC.test,
    Data.Datamining.Clustering.SGM6QC.test,
    Data.Datamining.Clustering.SGM4QC.test,
    Data.Datamining.Clustering.SOMQC.test,
    Data.Datamining.Clustering.DSOMQC.test
  ]

main :: IO ()
main = defaultMain tests
