{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module DAT.Row where 

import DAT.Math.Vector
import Data.List

data Row a = Row [a] deriving Show

instance Functor Row where
  fmap f (Row []) = Row []
  fmap f (Row (a:as)) = Row $ (f a) : (fmap f as)

instance Applicative Row where
  pure a = Row [a]

  (Row []) <*> _ = Row []
  (Row (f : fs)) <*> (Row [c]) = Row [f c]
  (Row (f : fs)) <*> (Row (c : cs)) = 
    Row [f c] `concatR` (Row fs <*> Row cs)

instance Monad Row where
  (>>=) (Row []) f = Row []
  (>>=) (Row c) f = joinR $ fmap f c

  return a = Row [a]
    
instance Foldable Row where
  foldr f acc (Row r) = foldr f acc r

instance Num a => Vector Row a where
  dotProduct (Row r1) (Row r2) 
    | length r1 /= length r2 = 
        error $ "Invalid number of vector dimensions in dot product"
    | otherwise = sum $ zipWith (*) r1 r2

  size (Row r) = length r

  genericSize (Row r) = genericLength r

joinR :: [(Row a)] -> Row a
joinR cols@((Row _):cs) = 
  foldl (\(Row acc) (Row [a]) -> Row (a:acc)) (Row []) (reverse cols)

concatR :: Row a -> Row a -> Row a
concatR (Row a) (Row b) = Row (a ++ b)

traverseR :: (a -> b) -> Row a -> Row b
traverseR f (Row r) = Row $ map f r

filterR :: (a -> Bool) -> Row a -> Row a
filterR f (Row r) = Row $ filter f r

zipWithR :: (a -> b -> c) -> Row a -> Row b -> Row c
zipWithR f (Row r1) (Row r2) = Row $ zipWith f r1 r2

zipR :: Row a -> Row b -> Row (a, b)
zipR r1 r2 = zipWithR (\x1 x2 -> (x1, x2)) r1 r2

headR :: Row a -> Row a
headR (Row r) = Row [(head r)]

tailR :: Row a -> Row a
tailR (Row r) = Row (tail r)

ith :: Row a -> Int -> a
ith (Row r) i = r !! i

eraseIth :: Int -> Row a -> Row a
eraseIth i (Row (a : as)) 
  | i == 0 = Row as
  | otherwise = eraseIth (i - 1) (Row as)

toRowOfType :: Row a -> (a -> b) -> Row b
toRowOfType r f = fmap f r

toString :: Show a => Row a -> String -> String
toString r sep = intercalate sep (toList (toRowOfType r show))

toList :: Row a -> [a]
toList (Row r) = r