{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DAT.Math.Statistics.Test.Rank where

import DAT.Row

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

inc :: Rank -> Rank
inc (Rank v) = Rank $ v + 1.0

rank :: Fractional a => (a -> a -> Bool) -> Row a -> Row Rank
rank eq (Row []) = Row []
rank eq (Row xs) = Row $ rank' eq xs Nothing NullRank []
  where
    rank' eq (x : xs) (Just prevVal) prevRank acc 
      | eq x prevVal = rank' eq xs (Just x) prevRank (prevRank : acc)
      | otherwise = rank' eq xs (Just x) (inc prevRank) ((inc prevRank) : acc)
    rank' eq (x : xs) prev NullRank acc =
      rank' eq xs (Just x) (Rank 1.0) ((Rank 1.0) : acc)
    rank' eq [] _ _ acc = reverse acc