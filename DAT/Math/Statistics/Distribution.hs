module DAT.Math.Statistics.Distribution where 


class Distribution d where
  cummulative :: d -> Double -> Double

class Distribution  d => DiscreteDistribution d where
  prob :: d -> Int -> Double

class Distribution d => ContinuousDistribution d where
  density :: d -> Double -> Double
