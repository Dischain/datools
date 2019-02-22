module DAT.Math.Statistics
(
  mean,
  sampleCovariance,
  stdDev,
  movingAvg,
  sampleVariance
) where

import DAT.Row
import DAT.Math.Probability.DiscreteRandomVar 

mean :: Row Double -> Double
mean (Row []) = error $ errMsg "can not calculate mean of an empty sample"
mean r = foldl (+) 0 r / (fromIntegral $ lengthR r)

-- | Sample covariance between two selections
sampleCovariance :: Row Double -> Row Double -> Double
sampleCovariance r1 r2 = mean ((*) <$> 
                              (fmap (\x1 -> x1 - mean1) r1) <*> 
                              (fmap (\x2 -> x2 - mean2) r2))
  where
    mean1 = mean r1
    mean2 = mean r2

sampleVariance :: Row Double -> Double
sampleVariance (Row []) = 
  error $ errMsg "can not calculate sample variance of an empty sample"
sampleVariance r = foldl (+) 0 (fmap (\x -> (x - mean') ** 2) r) / n
  where 
    mean' = mean r
    n = fromIntegral $ lengthR r

stdDev :: Row Double -> Double
stdDev (Row []) = 
  error $ errMsg "can not calculate standart deviation of an empty sample"
stdDev row@(Row values) = 
  sqrt ((sum $ map (\x -> (x - mu) ** 2) values) / (n - 1))
  where
    mu = mean row
    n = fromIntegral $ lengthR row

movingAvg :: Double -> Row Double -> Double
movingAvg a (Row []) = 0
movingAvg a (Row (x:xs)) = a * x + (1 - a) * (movingAvg a (Row xs))

covarianceMatrix :: Matrix m => m Double -> m Double
covarianceMatrix m 
  where 
    

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics: " ++ str