{-# LANGUAGE FlexibleContexts #-} 

module DAT.Math.Statistics.Test.Rank where

import DAT.Row

rank :: Fractional a => (a -> a -> Bool) -> Row a -> Row Double
rank eq (Row []) = Row []
rank eq (Row xs) = Row $ rank' eq xs Nothing Nothing []
  where
    rank' eq (x : xs) (Just prevVal) (Just prevRank) acc 
      | eq x prevVal = rank' eq xs (Just x) (Just $ prevRank) (prevRank : acc)
      | otherwise = rank' eq xs (Just x) (Just $ (prevRank + 1)) ((prevRank + 1) : acc)
    rank' eq (x : xs) prev Nothing acc =
      rank' eq xs (Just x) (Just 1.0) (1.0 : acc)
    rank' eq [] _ _ acc = reverse acc