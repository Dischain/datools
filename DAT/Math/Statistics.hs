module DAT.Math.Statistics
(
  mean,
  covariance
) where

import DAT.Row 

mean :: Row Double -> Double
mean (Row []) = error $ errMsg "can not calculate mean of an empty sample"
mean r = foldl (+) 0 r / (fromIntegral $ lengthR r)

expectation :: DRV -> Double

covariance :: Row Double -> Row Double -> Double
covariance r1 r2 = foldl (+) 0 
                         ((*) <$> 
                         (fmap (\x1 -> x1 - mean1) r1) <*> 
                         (fmap (\x2 -> x2 - mean2) r2))
  where
    mean1 = mean r1
    mean2 = mean r2

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics: " ++ str
