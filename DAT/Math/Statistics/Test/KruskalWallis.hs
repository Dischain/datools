module DAT.Math.Statistics.Test.KruskalWallis 
(
  KruskalWallisRes (..),
  kruskallWallis
) where

import DAT.Math.Statistics.Test.Rank
import DAT.Row hiding (fromList)
import DAT.Table
import Data.HashMap hiding (toList)
import Data.Hashable

data KruskalWallisRes = KruskalWallisRes {
  h :: Double,
  ndf :: Int
} deriving (Show, Eq)

kruskallWallis :: (Eq a, Ord a, Fractional a, Hashable a) => 
                  Table a -> KruskalWallisRes
kruskallWallis t = KruskalWallisRes { h = kwtest, ndf = ndf }
    where
      n = fromIntegral $ numItems t
      rankedS = rankedSample t
      a = 12 / (n * (n + 1))

      b_i :: (Row Rank, Int) -> Double
      b_i ((Row r), l) = (foldl (\acc i -> acc + toDouble i) 0 r) ** 2 / fromIntegral l

      c = 3 * (n + 1)

      rankedSampleWith_ni :: Table Rank -> [(Row Rank, Int)]
      rankedSampleWith_ni (ConsT r rs) = [(r, lengthR r)] ++ rankedSampleWith_ni rs
      rankedSampleWith_ni Empty = id []
   
      kwtest = a * foldl (\acc i -> acc + (b_i i)) 0 (rankedSampleWith_ni rankedS) - c

      ndf = numRows t - 1

rankedSample :: (Eq a, Ord a, Fractional a, Hashable a) => 
                Table a -> Table Rank
rankedSample t = mapRows (\r -> fmap (\a -> rm ! a) r) t
    where
      rm = rankedMap t 

rankedMap :: (Eq a, Ord a, Fractional a, Hashable a) => 
              Table a -> Map a Rank
rankedMap t = fromList $ toList (zipR asSortedRow (fractionalRank asSortedRow))
  where
    asSortedRow = sortR $ toRow t

errMsg :: String -> String
errMsg msg = "DAT.Math.Statistics.Test.KruskalWallis: " ++ msg