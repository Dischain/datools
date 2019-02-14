{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DAT.Math.Statistics.Test.Rank 
(
  Rank (..),
  inc,
  incN,
  dec,
  decN,
  r0, r1,
  standartRank,
  fractionalRank,
  rankedSample,
  rankedMap,
  toDouble
) where

import DAT.Row
import DAT.Table
import Data.List
import Data.HashMap hiding (toList, map)
import Data.Hashable

data Rank = NullRank | Rank Double deriving (Show, Eq)

instance Num Rank where
  (+) NullRank r = r
  (+) r NullRank = r
  (+) (Rank r1) (Rank r2) = Rank $ r1 + r2

  (-) NullRank r = r
  (-) r NullRank = r
  (-) (Rank r1) (Rank r2) = Rank $ r1 - r2

  (*) NullRank r = NullRank
  (*) r NullRank = NullRank
  (*) (Rank r1) (Rank r2) = Rank $ r1 * r2

  abs NullRank = NullRank
  abs (Rank r) = Rank $ abs r

  fromInteger i = Rank $ fromInteger i
  signum (Rank i) = Rank $ signum i

instance Ord Rank where
  NullRank `compare` NullRank = EQ
  NullRank `compare` Rank _ = LT
  Rank _ `compare` NullRank = GT
  Rank a `compare` Rank b = a `compare` b

instance Hashable Rank where
  hashWithSalt s (Rank r) = hashWithSalt s r
  hashWithSalt s NullRank = 0

inc :: Rank -> Rank
inc NullRank = Rank 1.0
inc (Rank v) = Rank $ v + 1.0

incN :: Rank -> Double -> Rank
incN (Rank a) n = Rank $ a + n
incN NullRank n = Rank n

dec :: Rank -> Rank
dec NullRank = error $ errMsg "rank can not be negative"
dec (Rank v) = Rank $ v - 1.0

decN :: Rank -> Double -> Rank
decN (Rank a) n 
  | n > a = error $ errMsg "rank can not be negative"
  | n == a = NullRank
  | otherwise = Rank $ a - n
decN NullRank n = error $ errMsg "null rank can not be decreased"

r0 :: Rank
r0 = NullRank

r1 :: Rank
r1 = Rank 1.0

standartRank :: Fractional a => (a -> a -> Bool) -> Row a -> Row Rank
standartRank eq (Row []) = Row []
standartRank eq (Row xs) = Row $ standartRank' eq xs Nothing NullRank []
  where
    standartRank' eq (x : xs) (Just prevVal) prevRank acc 
      | eq x prevVal = standartRank' eq xs (Just x) prevRank (prevRank : acc)
      | otherwise = standartRank' eq xs (Just x) (inc prevRank) ((inc prevRank) : acc)
    standartRank' eq (x : xs) prev NullRank acc =
      standartRank' eq xs (Just x) (Rank 1.0) ((Rank 1.0) : acc)
    standartRank' eq [] _ _ acc = reverse acc 

fractionalRank :: (Eq a, Fractional a) => Row a -> Row Rank
fractionalRank (Row []) = Row []
fractionalRank (Row r) = Row (reverse $ fractionalRank' (prep' r) r0 [])
  where
    fractionalRank' :: [Int] -> Rank -> [Rank] -> [Rank]
    fractionalRank' (x : xs) NullRank acc 
      | x == 1 = fractionalRank' xs r1 (r1 : acc)
      | otherwise = 
          fractionalRank' xs (Rank $ fromIntegral x) ((calcFractional' r0 x) ++ acc)
    fractionalRank' (x : xs) rnk acc
      | x == 1 = fractionalRank' xs (inc rnk) ((inc rnk) : acc)
      | otherwise = 
          fractionalRank' xs (rnk `incN` (fromIntegral x)) ((calcFractional' rnk x) ++ acc)
    fractionalRank' [] _ acc = acc

    prep' r = map (\x -> length x) (group r)

    calcFractional' NullRank n = 
      replicate n r1
    calcFractional' (Rank rnk) n = 
      replicate n (Rank (sum [(rnk + 1)..(rnk + fromIntegral n)] / fromIntegral n))

    newRank rnk x = rnk + (Rank $ fromIntegral (x + 1))

rankedSample :: (Eq a, Ord a, Fractional a, Hashable a) => 
                Table a -> (a -> a -> Bool) -> Table Rank
rankedSample t eq = 
  mapRows (\r -> fmap (\a -> rm ! a) r) t
    where
      rm = rankedMap t eq

rankedMap :: (Eq a, Ord a, Fractional a, Hashable a) => 
              Table a -> (a -> a -> Bool) -> Map a Rank
rankedMap t eq = fromList $ toList (zipR asSortedRow (fractionalRank asSortedRow))
  where
    asSortedRow = sortR $ toRow t

toDouble :: Rank -> Double
toDouble (Rank a) = a
toDouble NullRank = 0

errMsg :: String -> String
errMsg msg = "DAT.Math.Statistics.Test.Rank: " ++ msg