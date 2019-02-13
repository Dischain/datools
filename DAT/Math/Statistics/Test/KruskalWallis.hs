module DAT.Math.Statistics.Test.KruskalWallis where

import DAT.Math.Statistics.Test.Rank
import DAT.Row hiding (fromList)
import DAT.Table
import Data.HashMap hiding (toList)
import Data.Hashable

data KruskalWallisRes = KruskalWallisRes {
  h :: Double,
  ndf :: Int
}

kruskallWallis :: (Eq a, Ord a, Fractional a, Hashable a) => Table a -> (a -> a -> Bool) -> Double
kruskallWallis t eq = a * foldl (\acc i -> acc + (b_i i)) 0 (rankedSampleWith_ni rankedS) - c
  where
    n = fromIntegral $ numItems t
    rankedS = rankedSample t eq    
    a = 12 / (n * (n + 1))

    b_i :: (Row Rank, Int) -> Double
    b_i ((Row r), l) = (foldl (\acc i -> acc + toDouble i) 0 r) ** 2 / fromIntegral l

    c = 3 * (n + 1)

    rankedSampleWith_ni :: Table Rank -> [(Row Rank, Int)]
    rankedSampleWith_ni (ConsT r rs) = [(r, lengthR r)] ++ rankedSampleWith_ni rs
    rankedSampleWith_ni Empty = id []
 
rankedSample :: (Eq a, Ord a, Fractional a, Hashable a) => Table a -> (a -> a -> Bool) -> Table Rank
rankedSample t eq = 
  mapRows (\r -> fmap (\a -> rm ! a) r) t
    where
      rm = rankedMap t eq

rankedMap :: (Eq a, Ord a, Fractional a, Hashable a) => Table a -> (a -> a -> Bool) -> Map a Rank
rankedMap t eq = fromList $ toList (zipR asSortedRow (fractionalRank asSortedRow))
  where
    asSortedRow = sortR $ toRow t

-- import DAT.Row
-- import DAT.Table
-- import DAT.Math.Statistics.Test.KruskalWallis
-- import DAT.Math.Statistics.Test.Rank
-- let t = ConsT (Row [8.2, 10.3, 9.1, 12.6, 11.4, 13.2]) (ConsT (Row [10.2, 9.1, 13.9, 14.5, 9.1, 16.4]) (ConsT (Row [13.5, 8.4, 9.6, 13.8, 17.4, 15.3]) Empty))
-- let t = ConsT (Row [4, 2, 1, 1]) (ConsT (Row [2, 1, 4, 5]) (ConsT (Row [3, 5, 8, 7]) Empty))
