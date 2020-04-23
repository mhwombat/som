{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Data.Datamining.Clustering.SOM (adjustVector, euclideanDistanceSquared,
  normalise, Pattern(..), trainBatch)
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
import qualified Math.Geometry.GridMap as GM (lazyGridMap, map, GridMap, elems)

main = do
  -- Train it with the vectors from the image.
  let c2 = trainBatch learningRate c xs
  -- Save the SOM as an image.
  let c3 = GM.map (map round) c2
  print c3

-- This function calculates the learning rate to apply to a node based on its 
-- distance from the node that best matches the input. In this example, we
-- will only adjust the best node.
learningRate :: Int -> Double
learningRate d = exp (-d'*d'/4)
  where d' = fromIntegral d

---- Build a self-organising map (SOM) initialised with random values.
buildSOM :: RandomGen r => Rand r (GM.GridMap HexHexGrid (Int,Int) Pixel)
buildSOM = do
  ps <- sequence $ repeat emptyPattern
  return $ GM.lazyGridMap (hexHexGrid 2) ps

data Country = Country {
  name :: String,
  medianAgeOfPopulation :: Double,
  childDependencyRatio :: Double,
  oldAgeDependencyRatio :: Double,
  totalDependencyRatio :: Double,
  lifeExpectancyAt75 :: Double,
  adultSurvival :: Double,


 }
Country / Areas (1),
Median Age of the Population (2),
Child Dependency Ratio (3),
Old Age Dependency Ratio (4),Total Dependency Ratio (5),Life Expectancy at Age 75 (6),"Adult
Survival 
(7)",Scaled Median Age of the Population (2),Scaled Child Dependency Ratio (3),Scaled Old Age Dependency Ratio (4),Scaled Total Dependency Ratio (5),Scaled Life Expectancy at Age 75 (6),"Scaled Adult
Survival 
(7)"
Afghanistan,16.6,143,6,149,6.6,926,0.0376712329,0.9037037037,0.1351351351,0.9,0.2526315789,0.9707401033
Albania,30,54,16,70,9.7,926,0.4965753425,0.2444444444,0.4054054054,0.3357142857,0.5789473684,0.9707401033
Algeria,26.2,64,8,72,8.7,879,0.3664383562,0.3185185185,0.1891891892,0.35,0.4736842105,0.8898450947
Angola,16.6,143,6,149,7.2,625,0.0376712329,0.9037037037,0.1351351351,0.9,0.3157894737,0.4526678141
Argentina,30.4,59,19,78,10.7,878,0.5102739726,0.2814814815,0.4864864865,0.3928571429,0.6842105263,0.8881239243
Armenia,32.1,48,18,66,10.2,879,0.5684931507,0.2,0.4594594595,0.3071428571,0.6315789474,0.8898450947
Aruba,38.3,42,15,57,9.2,898,0.7808219178,0.1555555556,0.3783783784,0.2428571429,0.5263157895,0.9225473322
Australia,36.9,42,22,64,12.5,935,0.7328767123,0.1555555556,0.5675675676,0.2928571429,0.8736842105,0.9862306368
Austria,41.8,34,29,62,11.7,926,0.9006849315,0.0962962963,0.7567567568,0.2785714286,0.7894736842,0.9707401033
Azerbaijan,29.5,49,10,60,8.6,859,0.4794520548,0.2074074074,0.2432432432,0.2642857143,0.4631578947,0.8554216867
Bahamas,30.9,51,11,62,9.9,887,0.5273972603,0.2222222222,0.2702702703,0.2785714286,0.6,0.9036144578
Bahrain,30.1,35,3,38,8.1,909,0.5,0.1037037037,0.0540540541,0.1071428571,0.4105263158,0.9414802065
Bangladesh,24.2,78,9,86,8.5,842,0.2979452055,0.4222222222,0.2162162162,0.45,0.4526315789,0.82616179
Barbados,37.5,39,18,57,9.9,910,0.7534246575,0.1333333333,0.4594594595,0.2428571429,0.6,0.9432013769
Belarus,38.3,33,21,54,8.9,762,0.7808219178,0.0888888889,0.5405405405,0.2214285714,0.4947368421,0.6884681583
Belgium,41.2,38,29,67,11.6,917,0.8801369863,0.1259259259,0.7567567568,0.3142857143,0.7789473684,0.9552495697
Belize,21.8,94,8,102,10.8,882,0.2157534247,0.5407407407,0.1891891892,0.5642857143,0.6947368421,0.8950086059
Benin,17.9,128,7,135,7.6,684,0.0821917808,0.7925925926,0.1621621622,0.8,0.3578947368,0.5542168675
Bhutan,24.6,73,9,82,8.5,801,0.3116438356,0.3851851852,0.2162162162,0.4214285714,0.4526315789,0.7555938038
Bolivia (Plurinational State of),21.7,97,10,106,8.3,794,0.2123287671,0.562962963,0.2432432432,0.5928571429,0.4315789474,0.743545611
Bosnia and Herzegovina,39.4,32,22,54,9.7,894,0.8184931507,0.0814814815,0.5675675676,0.2214285714,0.5789473684,0.9156626506
Botswana,22.9,83,8,91,8,481,0.2534246575,0.4592592593,0.1891891892,0.4857142857,0.4,0.2048192771
Brazil,29.1,57,12,69,11.5,823,0.4657534247,0.2666666667,0.2972972973,0.3285714286,0.7684210526,0.7934595525
Brunei Darussalam,28.9,55,6,61,12.5,887,0.4589041096,0.2518518519,0.1351351351,0.2714285714,0.8736842105,0.9036144578
Bulgaria,41.6,30,28,58,8.7,845,0.8938356164,0.0666666667,0.7297297297,0.25,0.4736842105,0.8313253012
Burkina Faso,17.1,135,5,141,6.2,717,0.0547945205,0.8444444444,0.1081081081,0.8428571429,0.2105263158,0.6110154905
Burundi,20.2,104,6,110,7.3,579,0.1609589041,0.6148148148,0.1351351351,0.6214285714,0.3263157895,0.3734939759
Cambodia,22.9,85,7,92,7.5,747,0.2534246575,0.4740740741,0.1621621622,0.4928571429,0.3473684211,0.6626506024
Cameroon,19.3,115,8,122,7.4,587,0.1301369863,0.6962962963,0.1891891892,0.7071428571,0.3368421053,0.3872633391
Canada,39.9,37,22,59,12.4,925,0.8356164384,0.1185185185,0.5675675676,0.2571428571,0.8631578947,0.9690189329
Cape Verde,22.8,87,12,99,9.1,883,0.25,0.4888888889,0.2972972973,0.5428571429,0.5157894737,0.8967297762
Central African Republic,19.4,114,9,123,7.2,509,0.1335616438,0.6888888889,0.2162162162,0.7142857143,0.3157894737,0.2530120482
Chad,17.1,137,7,144,6.4,646,0.0547945205,0.8592592593,0.1621621622,0.8642857143,0.2315789474,0.4888123924
Channel Islands,42.6,32,26,58,11.1,939,0.9280821918,0.0814814815,0.6756756757,0.25,0.7263157895,0.9931153184
Chile,32.1,51,15,67,12.2,906,0.5684931507,0.2222222222,0.3783783784,0.3142857143,0.8421052632,0.9363166954
China,34.5,42,13,55,8.9,880,0.6506849315,0.1555555556,0.3243243243,0.2285714286,0.4947368421,0.8915662651
"China, Hong Kong SAR",41.8,26,18,45,12.6,940,0.9006849315,0.037037037,0.4594594595,0.1571428571,0.8842105263,0.9948364888
"China, Macao SAR",37.6,28,10,38,10.9,940,0.7568493151,0.0518518519,0.2432432432,0.1071428571,0.7052631579,0.9948364888
Colombia,26.8,68,10,78,10.6,852,0.3869863014,0.3481481481,0.2432432432,0.3928571429,0.6736842105,0.843373494
Comoros,18.9,115,6,121,7.3,731,0.1164383562,0.6962962963,0.1351351351,0.7,0.3263157895,0.6351118761
Congo,19.6,112,8,120,7.9,665,0.1404109589,0.6740740741,0.1891891892,0.6928571429,0.3894736842,0.5215146299
Costa Rica,28.4,57,11,68,12.2,913,0.4417808219,0.2666666667,0.2702702703,0.3214285714,0.8421052632,0.9483648881
Côte d'Ivoire,19.2,117,9,125,7.7,601,0.1267123288,0.7111111111,0.2162162162,0.7285714286,0.3684210526,0.4113597246
Croatia,41.5,34,28,61,9.7,893,0.8904109589,0.0962962963,0.7297297297,0.2714285714,0.5789473684,0.9139414802
Cuba,38.4,38,20,58,11.7,905,0.7842465753,0.1259259259,0.5135135135,0.25,0.7894736842,0.934595525
Cyprus,34.2,40,18,59,10.1,938,0.6404109589,0.1407407407,0.4594594595,0.2571428571,0.6210526316,0.991394148
Czech Republic,39.4,31,23,54,10.1,895,0.8184931507,0.0740740741,0.5945945946,0.2214285714,0.6210526316,0.917383821
Dem. People's Rep. of Korea,32.9,53,16,69,7.8,835,0.595890411,0.237037037,0.4054054054,0.3285714286,0.3789473684,0.8141135972
Dem. Republic of the Congo,16.7,143,7,150,7,611,0.0410958904,0.9037037037,0.1621621622,0.9071428571,0.2947368421,0.4285714286
Denmark,40.6,41,28,69,10.9,907,0.8595890411,0.1481481481,0.7297297297,0.3285714286,0.7052631579,0.9380378657
Djibouti,21.4,95,7,102,7.2,685,0.2020547945,0.5481481481,0.1621621622,0.5642857143,0.3157894737,0.5559380379
Dominican Republic,25.1,77,12,89,12.2,828,0.3287671233,0.4148148148,0.2972972973,0.4714285714,0.8421052632,0.8020654045
Ecuador,25.5,74,12,86,11.5,872,0.3424657534,0.3925925926,0.2972972973,0.45,0.7684210526,0.8777969019
Egypt,24.4,77,9,86,8.2,880,0.3047945205,0.4148148148,0.2162162162,0.45,0.4210526316,0.8915662651
El Salvador,23.2,88,14,103,11.9,797,0.2636986301,0.4962962963,0.3513513514,0.5714285714,0.8105263158,0.7487091222
Equatorial Guinea,20.3,104,6,110,7.3,631,0.1643835616,0.6148148148,0.1351351351,0.6214285714,0.3263157895,0.4629948365
Eritrea,19,114,5,120,7.5,681,0.1198630137,0.6888888889,0.1081081081,0.6928571429,0.3473684211,0.5490533563
Estonia,39.7,34,28,62,10.6,826,0.8287671233,0.0962962963,0.7297297297,0.2785714286,0.6736842105,0.7986230637
Ethiopia,18.7,122,8,130,7.9,699,0.1095890411,0.7481481481,0.1891891892,0.7642857143,0.3894736842,0.5800344234
Fiji,26.4,68,9,77,8.2,793,0.3732876712,0.3481481481,0.2162162162,0.3857142857,0.4210526316,0.7418244406
Finland,42,38,29,67,11.8,903,0.9075342466,0.1259259259,0.7567567568,0.3142857143,0.8,0.9311531842
France,39.9,41,29,70,12.9,913,0.8356164384,0.1481481481,0.7567567568,0.3357142857,0.9157894737,0.9483648881
French Guiana,24.3,81,8,90,9.8,906,0.301369863,0.4444444444,0.1891891892,0.4785714286,0.5894736842,0.9363166954
French Polynesia,29.1,58,11,68,9.5,883,0.4657534247,0.2740740741,0.2702702703,0.3214285714,0.5578947368,0.8967297762
Gabon,21.6,96,9,105,8.5,714,0.2089041096,0.5555555556,0.2162162162,0.5857142857,0.4526315789,0.6058519793
Gambia,17.8,128,5,133,5.1,716,0.0787671233,0.7925925926,0.1081081081,0.7857142857,0.0947368421,0.6092943201
Georgia,37.3,40,23,63,9.3,877,0.7465753425,0.1407407407,0.5945945946,0.2857142857,0.5368421053,0.8864027539
Germany,44.3,30,33,64,11.5,921,0.9863013699,0.0666666667,0.8648648649,0.2928571429,0.7684210526,0.9621342513
Ghana,20.5,104,8,112,8.5,745,0.1712328767,0.6148148148,0.1891891892,0.6357142857,0.4526315789,0.6592082616
Greece,41.4,31,30,61,11.1,924,0.8869863014,0.0740740741,0.7837837838,0.2714285714,0.7263157895,0.9672977625
Grenada,25,70,13,83,9.3,903,0.3253424658,0.362962963,0.3243243243,0.4285714286,0.5368421053,0.9311531842
Guadeloupe,36.8,52,22,74,12.9,902,0.7294520548,0.2296296296,0.5675675676,0.3642857143,0.9157894737,0.9294320138
Guam,29.2,64,12,77,8.9,896,0.4691780822,0.3185185185,0.2972972973,0.3857142857,0.4947368421,0.9191049914
Guatemala,18.8,122,10,132,10.2,820,0.1130136986,0.7481481481,0.2432432432,0.7785714286,0.6315789474,0.7882960413
Guinea,18.3,124,8,132,7.4,656,0.095890411,0.762962963,0.1891891892,0.7785714286,0.3368421053,0.5060240964
Guinea-Bissau,19,117,7,124,7,604,0.1198630137,0.7111111111,0.1621621622,0.7214285714,0.2947368421,0.4165232358
Guyana,23.8,85,8,93,9.2,828,0.2842465753,0.4740740741,0.1891891892,0.5,0.5263157895,0.8020654045
Haiti,21.5,96,9,105,7.2,737,0.2054794521,0.5555555556,0.2162162162,0.5857142857,0.3157894737,0.6454388985
Honduras,21,101,9,110,10.8,853,0.1883561644,0.5925925926,0.2162162162,0.6214285714,0.6947368421,0.8450946644
Hungary,39.8,33,26,59,9.8,830,0.8321917808,0.0888888889,0.6756756757,0.2571428571,0.5894736842,0.8055077453
Iceland,34.8,47,20,68,11.9,941,0.6609589041,0.1925925926,0.5135135135,0.3214285714,0.8105263158,0.9965576592
India,25.1,74,9,83,8.4,779,0.3287671233,0.3925925926,0.2162162162,0.4285714286,0.4421052632,0.7177280551
Indonesia,27.8,62,10,71,8,795,0.4212328767,0.3037037037,0.2432432432,0.3428571429,0.4,0.7452667814
Iran (Islamic Republic of),27.1,52,8,61,9.4,869,0.397260274,0.2296296296,0.1891891892,0.2714285714,0.5473684211,0.8726333907
Iraq,18.2,125,8,132,7,822,0.0924657534,0.7703703704,0.1891891892,0.7785714286,0.2947368421,0.7917383821
Ireland,34.7,45,19,64,11.3,926,0.6575342466,0.1777777778,0.4864864865,0.2928571429,0.7473684211,0.9707401033
Israel,30.1,64,19,83,11.9,934,0.5,0.3185185185,0.4864864865,0.4285714286,0.8105263158,0.9845094664
Italy,43.2,31,34,65,12.2,937,0.948630137,0.0740740741,0.8918918919,0.3,0.8421052632,0.9896729776
Jamaica,27,73,15,87,10.6,835,0.3938356164,0.3851851852,0.3783783784,0.4571428571,0.6736842105,0.8141135972
Japan,44.7,31,38,69,13.7,934,1,0.0740740741,1,0.3285714286,1,0.9845094664
Jordan,20.7,102,8,110,8.9,874,0.1780821918,0.6,0.1891891892,0.6214285714,0.4947368421,0.8812392427
Kazakhstan,29,55,11,67,7.9,733,0.4623287671,0.2518518519,0.2702702703,0.3142857143,0.3894736842,0.6385542169
Kenya,18.5,120,6,126,8.1,602,0.102739726,0.7333333333,0.1351351351,0.7357142857,0.4105263158,0.413080895
Kuwait,28.2,52,4,56,7.7,910,0.4349315068,0.2296296296,0.0810810811,0.2357142857,0.3684210526,0.9432013769
Kyrgyzstan,23.8,76,8,84,8,779,0.2842465753,0.4074074074,0.1891891892,0.4357142857,0.4,0.7177280551
Lao People's Dem. Republic,21.5,95,8,103,7.7,801,0.2054794521,0.5481481481,0.1891891892,0.5714285714,0.3684210526,0.7555938038
Latvia,40.2,32,29,60,9.9,802,0.845890411,0.0814814815,0.7567567568,0.2642857143,0.6,0.7573149742
Lebanon,29.1,58,12,70,8.4,869,0.4657534247,0.2740740741,0.2972972973,0.3357142857,0.4421052632,0.8726333907
Lesotho,20.3,107,9,116,7.5,394,0.1643835616,0.637037037,0.2162162162,0.6642857143,0.3473684211,0.0550774527
Liberia,18.2,124,6,130,7.1,643,0.0924657534,0.762962963,0.1351351351,0.7642857143,0.3052631579,0.4836488812
Libyan Arab Jamahiriya,25.9,69,8,76,9.4,880,0.3561643836,0.3555555556,0.1891891892,0.3785714286,0.5473684211,0.8915662651
Lithuania,39.3,35,26,61,9.7,781,0.8150684932,0.1037037037,0.6756756757,0.2714285714,0.5789473684,0.7211703959
Luxembourg,38.9,38,22,60,11.4,916,0.801369863,0.1259259259,0.5675675676,0.2642857143,0.7578947368,0.9535283993
Madagascar,18.2,126,7,133,8.5,802,0.0924657534,0.7777777778,0.1621621622,0.7857142857,0.4526315789,0.7573149742
Malawi,16.9,143,8,151,7.2,557,0.0479452055,0.9037037037,0.1891891892,0.9142857143,0.3157894737,0.3356282272
Malaysia,26,71,9,80,7.9,880,0.3595890411,0.3703703704,0.2162162162,0.4071428571,0.3894736842,0.8915662651
Maldives,24.6,69,9,78,8.9,918,0.3116438356,0.3555555556,0.2162162162,0.3928571429,0.4947368421,0.9569707401
Mali,16.3,145,6,150,5.7,662,0.0273972603,0.9185185185,0.1351351351,0.9071428571,0.1578947368,0.5163511188
Malta,39.5,34,22,56,10.5,932,0.8219178082,0.0962962963,0.5675675676,0.2357142857,0.6631578947,0.9810671256
Martinique,39.4,45,25,70,12.5,920,0.8184931507,0.1777777778,0.6486486486,0.3357142857,0.8736842105,0.9604130809
Mauritania,19.8,108,6,114,5.7,736,0.147260274,0.6444444444,0.1351351351,0.65,0.1578947368,0.6437177281
Mauritius,32.4,49,11,60,10.1,840,0.5787671233,0.2074074074,0.2702702703,0.2642857143,0.6210526316,0.8227194492
Mayotte,17.2,126,4,131,12.5,859,0.0582191781,0.7777777778,0.0810810811,0.7714285714,0.8736842105,0.8554216867
Mexico,26.6,71,12,82,11.3,891,0.3801369863,0.3703703704,0.2972972973,0.4214285714,0.7473684211,0.9104991394
Micronesia (Fed. States of),20.8,101,8,108,7.9,826,0.1815068493,0.5925925926,0.1891891892,0.6071428571,0.3894736842,0.7986230637
Mongolia,25.4,65,7,72,8.9,768,0.3390410959,0.3259259259,0.1621621622,0.35,0.4947368421,0.6987951807
Montenegro,35.9,42,20,62,9,865,0.698630137,0.1555555556,0.5135135135,0.2785714286,0.5052631579,0.8657487091
Morocco,26.3,67,10,77,8.1,875,0.3698630137,0.3407407407,0.2432432432,0.3857142857,0.4105263158,0.8829604131
Mozambique,17.8,130,8,138,7.6,530,0.0787671233,0.8074074074,0.1891891892,0.8214285714,0.3578947368,0.2891566265
Myanmar,28.2,58,9,67,7.5,773,0.4349315068,0.2740740741,0.2162162162,0.3142857143,0.3473684211,0.7074010327
Namibia,21.2,98,8,106,8.1,658,0.1952054795,0.5703703704,0.1891891892,0.5928571429,0.4105263158,0.5094664372
Nepal,21.4,97,9,106,7.8,816,0.2020547945,0.562962963,0.2162162162,0.5928571429,0.3789473684,0.7814113597
Netherlands,40.7,39,25,64,11.4,933,0.8630136986,0.1333333333,0.6486486486,0.2928571429,0.7578947368,0.982788296
Netherlands Antilles,37.9,41,16,58,11.2,885,0.7671232877,0.1481481481,0.4054054054,0.25,0.7368421053,0.900172117
New Caledonia,30.3,58,14,72,11.1,869,0.5068493151,0.2740740741,0.3513513514,0.35,0.7263157895,0.8726333907
New Zealand,36.6,47,22,69,12,925,0.7226027397,0.1925925926,0.5675675676,0.3285714286,0.8210526316,0.9690189329
Nicaragua,22.1,93,9,102,11.4,838,0.2260273973,0.5333333333,0.2162162162,0.5642857143,0.7578947368,0.8192771084
Niger,15.5,154,6,160,5.8,694,0,0.9851851852,0.1351351351,0.9785714286,0.1684210526,0.5714285714
Nigeria,18.5,122,8,130,7.4,604,0.102739726,0.7481481481,0.1891891892,0.7642857143,0.3368421053,0.4165232358
Norway,38.7,43,25,67,11.7,931,0.7945205479,0.162962963,0.6486486486,0.3142857143,0.7894736842,0.9793459552
Occupied Palestinian Territory,18.1,128,6,134,8.4,870,0.0890410959,0.7925925926,0.1351351351,0.7928571429,0.4421052632,0.8743545611
Oman,25.3,61,4,65,7.2,880,0.3356164384,0.2962962963,0.0810810811,0.3,0.3157894737,0.8915662651
Pakistan,21.7,95,9,104,8.1,822,0.2123287671,0.5481481481,0.2162162162,0.5785714286,0.4105263158,0.7917383821
Panama,27.3,68,12,80,10.6,894,0.404109589,0.3481481481,0.2972972973,0.4071428571,0.6736842105,0.9156626506
Papua New Guinea,20.4,103,6,108,7.5,706,0.1678082192,0.6074074074,0.1351351351,0.6071428571,0.3473684211,0.5920826162
Paraguay,23.1,86,10,96,10.3,850,0.2602739726,0.4814814815,0.2432432432,0.5214285714,0.6421052632,0.8399311532
Peru,25.6,74,11,85,10.6,866,0.345890411,0.3925925926,0.2702702703,0.4428571429,0.6736842105,0.8674698795
Philippines,22.2,91,7,98,7.9,783,0.2294520548,0.5185185185,0.1621621622,0.5357142857,0.3894736842,0.7246127367
Poland,38,33,21,54,10.5,861,0.7705479452,0.0888888889,0.5405405405,0.2214285714,0.6631578947,0.8588640275
Portugal,41,33,29,62,11,909,0.8732876712,0.0888888889,0.7567567568,0.2785714286,0.7157894737,0.9414802065
Puerto Rico,34.4,50,22,72,12.1,904,0.647260274,0.2148148148,0.5675675676,0.35,0.8315789474,0.9328743546
Qatar,31.6,21,1,23,11,932,0.551369863,0,0,0,0.7157894737,0.9810671256
Republic of Korea,37.9,36,17,53,11.7,922,0.7671232877,0.1111111111,0.4324324324,0.2142857143,0.7894736842,0.9638554217
Republic of Moldova,35.2,39,17,56,7.9,771,0.6746575342,0.1333333333,0.4324324324,0.2357142857,0.3894736842,0.7039586919
Réunion,29.9,58,14,73,12.4,861,0.4931506849,0.2740740741,0.3513513514,0.3571428571,0.8631578947,0.8588640275
Romania,38.4,32,23,55,9.1,855,0.7842465753,0.0814814815,0.5945945946,0.2285714286,0.5157894737,0.8485370052
Russian Federation,37.9,31,19,51,9.2,727,0.7671232877,0.0740740741,0.4864864865,0.2,0.5263157895,0.6282271945
Rwanda,18.7,118,6,124,7.9,660,0.1095890411,0.7185185185,0.1351351351,0.7214285714,0.3894736842,0.512908778
Saint Lucia,27.4,62,12,74,10.6,862,0.4075342466,0.3037037037,0.2972972973,0.3642857143,0.6736842105,0.8605851979
St. Vincent and the Grenadines,27.9,63,12,75,8.4,864,0.4246575342,0.3111111111,0.2972972973,0.3714285714,0.4421052632,0.8640275387
Samoa,20.9,105,11,116,9.3,848,0.1849315068,0.6222222222,0.2702702703,0.6642857143,0.5368421053,0.8364888124
Sao Tome and Principe,19.3,116,9,125,8.4,784,0.1301369863,0.7037037037,0.2162162162,0.7285714286,0.4421052632,0.7263339071
Saudi Arabia,25.9,68,5,74,8.7,877,0.3561643836,0.3481481481,0.1081081081,0.3642857143,0.4736842105,0.8864027539
Senegal,17.8,128,6,134,5,725,0.0787671233,0.7925925926,0.1351351351,0.7928571429,0.0842105263,0.6247848537
Serbia,37.6,39,23,63,9,878,0.7568493151,0.1333333333,0.5945945946,0.2857142857,0.5052631579,0.8881239243
Sierra Leone,18.4,120,4,124,4.2,529,0.0993150685,0.7333333333,0.0810810811,0.7214285714,0,0.2874354561
Singapore,37.6,37,14,51,12.1,935,0.7568493151,0.1185185185,0.3513513514,0.2,0.8315789474,0.9862306368
Slovakia,36.9,33,18,52,9.5,866,0.7328767123,0.0888888889,0.4594594595,0.2071428571,0.5578947368,0.8674698795
Slovenia,41.7,30,26,55,11.2,908,0.897260274,0.0666666667,0.6756756757,0.2285714286,0.7368421053,0.9397590361
Solomon Islands,19.9,108,7,114,7.7,805,0.1506849315,0.6444444444,0.1621621622,0.65,0.3684210526,0.7624784854
Somalia,17.5,130,6,136,7.1,652,0.0684931507,0.8074074074,0.1351351351,0.8071428571,0.3052631579,0.4991394148
South Africa,24.9,73,8,81,8.1,467,0.3219178082,0.3851851852,0.1891891892,0.4142857143,0.4105263158,0.1807228916
Spain,40.1,31,27,58,11.9,924,0.8424657534,0.0740740741,0.7027027027,0.25,0.8105263158,0.9672977625
Sri Lanka,30.7,55,14,69,10.2,861,0.5205479452,0.2518518519,0.3513513514,0.3285714286,0.6315789474,0.8588640275
Sudan,19.7,110,8,118,7.3,752,0.1438356164,0.6592592593,0.1891891892,0.6785714286,0.3263157895,0.6712564544
Suriname,27.6,65,11,77,9.2,811,0.4143835616,0.3259259259,0.2702702703,0.3857142857,0.5263157895,0.7728055077
Swaziland,19.5,113,7,121,7.5,431,0.1369863014,0.6814814815,0.1621621622,0.7,0.3473684211,0.1187607573
Sweden,40.7,40,31,71,11.7,936,0.8630136986,0.1407407407,0.8108108108,0.3428571429,0.7894736842,0.9879518072
Switzerland,41.4,34,27,61,12.5,943,0.8869863014,0.0962962963,0.7027027027,0.2714285714,0.8736842105,1
Syrian Arab Republic,21.1,100,8,108,9.3,902,0.1917808219,0.5851851852,0.1891891892,0.6071428571,0.5368421053,0.9294320138
Tajikistan,20.4,104,7,111,9.2,816,0.1678082192,0.6148148148,0.1621621622,0.6285714286,0.5263157895,0.7814113597
TFYR Macedonia,35.9,40,19,58,8.9,892,0.698630137,0.1407407407,0.4864864865,0.25,0.4947368421,0.9122203098
Thailand,34.2,45,14,59,10.3,839,0.6404109589,0.1777777778,0.3513513514,0.2571428571,0.6421052632,0.8209982788
Timor-Leste,16.6,148,8,156,7.4,743,0.0376712329,0.9407407407,0.1891891892,0.95,0.3368421053,0.6557659208
Togo,19.7,110,7,118,7.8,665,0.1438356164,0.6592592593,0.1621621622,0.6785714286,0.3789473684,0.5215146299
Tonga,21.3,104,13,117,9.4,853,0.198630137,0.6148148148,0.3243243243,0.6714285714,0.5473684211,0.8450946644
Trinidad and Tobago,30.8,44,11,55,9.3,809,0.5239726027,0.1703703704,0.2702702703,0.2285714286,0.5368421053,0.769363167
Tunisia,28.9,54,11,65,8.4,898,0.4589041096,0.2444444444,0.2702702703,0.3,0.4421052632,0.9225473322
Turkey,28.3,60,10,70,8.4,886,0.4383561644,0.2888888889,0.2432432432,0.3357142857,0.4421052632,0.9018932874
Turkmenistan,24.5,73,7,80,8.4,764,0.3082191781,0.3851851852,0.1621621622,0.4071428571,0.4421052632,0.6919104991
Uganda,15.7,156,7,163,7.8,585,0.0068493151,1,0.1621621622,1,0.3789473684,0.3838209983
Ukraine,39.3,32,24,56,8.5,729,0.8150684932,0.0814814815,0.6216216216,0.2357142857,0.4526315789,0.6316695353
United Arab Emirates,30.1,30,1,30,9.7,914,0.5,0.0666666667,0,0.05,0.5789473684,0.9500860585
United Kingdom,39.8,40,28,67,11.6,921,0.8321917808,0.1407407407,0.7297297297,0.3142857143,0.7789473684,0.9621342513
United Republic of Tanzania,17.5,133,8,140,8.1,615,0.0684931507,0.8296296296,0.1891891892,0.8357142857,0.4105263158,0.4354561102
United States of America,36.9,45,22,67,11.9,889,0.7328767123,0.1777777778,0.5675675676,0.3142857143,0.8105263158,0.9070567986
United States Virgin Islands,38.8,50,25,75,11.9,926,0.7979452055,0.2148148148,0.6486486486,0.3714285714,0.8105263158,0.9707401033
Uruguay,33.7,54,25,79,10.8,897,0.6232876712,0.2444444444,0.6486486486,0.4,0.6947368421,0.9208261618
Uzbekistan,24.2,75,8,83,9.5,807,0.2979452055,0.4,0.1891891892,0.4285714286,0.5578947368,0.7659208262
Vanuatu,20.6,102,7,110,8,846,0.1746575342,0.6,0.1621621622,0.6214285714,0.4,0.8330464716
Venezuela (Bolivarian Republic of),26.1,70,10,80,10.4,864,0.3630136986,0.362962963,0.2432432432,0.4071428571,0.6526315789,0.8640275387
Viet Nam,28.2,56,10,66,10.1,883,0.4349315068,0.2592592593,0.2432432432,0.3071428571,0.6210526316,0.8967297762
Western Sahara,26.5,62,4,66,7.7,798,0.3767123288,0.3037037037,0.0810810811,0.3071428571,0.3684210526,0.7504302926
Yemen,17.4,136,6,142,7.6,778,0.0650684932,0.8518518519,0.1351351351,0.85,0.3578947368,0.7160068847
Zambia,16.7,143,8,151,7.5,498,0.0410958904,0.9037037037,0.1891891892,0.9142857143,0.3473684211,0.2340791738
Zimbabwe,19.3,118,10,127,8.5,362,0.1301369863,0.7185185185,0.2432432432,0.7428571429,0.4526315789,0

