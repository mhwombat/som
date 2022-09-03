------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests
--
------------------------------------------------------------------------
import Data.Datamining.Clustering.DSOMQC (test)
import Data.Datamining.Clustering.SGM4QC (test)
import Data.Datamining.Clustering.SGMQC  (test)
import Data.Datamining.Clustering.SOMQC  (test)
import Data.Datamining.Pattern.ListQC    (test)
import Data.Datamining.Pattern.NumericQC (test)

import Test.Framework                    as TF (Test, defaultMain)

tests :: [TF.Test]
tests =
  [
    Data.Datamining.Pattern.NumericQC.test,
    Data.Datamining.Pattern.ListQC.test,
    Data.Datamining.Clustering.SGMQC.test,
    Data.Datamining.Clustering.SGM4QC.test,
    Data.Datamining.Clustering.SOMQC.test,
    Data.Datamining.Clustering.DSOMQC.test
  ]

main :: IO ()
main = defaultMain tests
