module DAT.Math.Statistics
(
  mean,
  sampleCovariance,
  stdDev
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

stdDev :: Row Double -> Double
stdDev (Row []) = 
  error $ errMsg "can not calculate standart deviation of an empty sample"
stdDev row@(Row values) = 
  sqrt ((sum $ map (\x -> (x - mu) ** 2) values) / (n - 1))
  where
    mu = mean row
    n = fromIntegral $ lengthR row

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics: " ++ str