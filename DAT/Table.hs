{-# LANGUAGE FlexibleContexts #-} 

module DAT.Table where

import DAT.Row
import Text.CSV
import Control.Monad
import Database.HDBC
import Data.List

import Text.Regex.Posix 
import Text.Regex.Posix.Wrap 
import Text.Regex.Base.RegexLike

data Table a = Empty | ConsT (Row a) (Table a) deriving Show

instance Functor Table where
  fmap f Empty = Empty
  fmap f (ConsT c cs) = ConsT (fmap f c) (fmap f cs)
  
instance Applicative Table where
  pure a = ConsT (pure a) Empty
  
  Empty <*> _ = Empty
  (ConsT fs fss) <*> t@(ConsT c Empty) = ConsT (fs <*> c) Empty
  (ConsT fs fss) <*> (ConsT bs bss) =
    (ConsT (fs <*> bs) Empty) `appendT` (fss <*> bss)
  
instance Monad Table where
  (>>=) Empty f = Empty
  (>>=) (ConsT r Empty) f = case (fmap f r) of 
    Row tbls@(t:ts) -> joinT tbls
    Row [] -> Empty
  (>>=) (ConsT r rs) f  = appendT ((ConsT r Empty) >>= f) (rs >>= f)
  
  return a = ConsT (Row [a]) Empty
  
appendT :: Table a -> Table a -> Table a
appendT Empty Empty = Empty
appendT t1 Empty = t1
appendT Empty t2 = t2
appendT (ConsT a as) bs = ConsT a (appendT as bs)
  
prependT :: Table a -> Table a -> Table a
prependT which to = appendT to which

-- Concatenating table horizontally
concatT :: Table a -> Table a -> Table a
concatT Empty Empty = Empty
concatT t1 Empty = t1
concatT Empty t2 = t2
concatT (ConsT a as) (ConsT b bs) = ConsT (concatR a b) (concatT as bs)
  
joinT :: [Table a] -> Table a
joinT tbls@((ConsT c cs) : ds) =
    foldl (\acc t-> appendT t acc) Empty (reverse tbls)
  
mkTable :: [[a]] -> Table a
mkTable t@(r@(c : cs) : rs) = 
  (foldl (\acc r -> ConsT r acc) Empty) (reverse $ map (\r -> Row r) t)
mkTable [[]] = Empty

headTbl :: Table a -> Table a
headTbl (ConsT r _) = ConsT r Empty
headTbl Empty = Empty
  
tailTbl :: Table a -> Table a
tailTbl (ConsT h t@(ConsT _ _)) = t
tailTbl Empty = Empty
  
ithRow :: Int -> Table a -> Maybe (Table a)
ithRow i (ConsT r t)
  | i < 0  = Nothing
  | i == 0 = Just $ ConsT r Empty
  | i > 0 = case t of (ConsT nr _) -> ithRow (i - 1) t
                      Empty -> Nothing
ithRow _ Empty = Nothing

eraseEmptyRows :: Table a -> Table a
eraseEmptyRows Empty = Empty
eraseEmptyRows (ConsT (Row []) rs) = rs
eraseEmptyRows (ConsT r@(Row (a : as)) rs) = ConsT r (eraseEmptyRows rs)

ithCol :: Int -> Table a -> Maybe (Table a)
ithCol i t@(ConsT r rs)
  | i < 0 = Nothing
  | otherwise = Just $ modifyRows (\r -> Row [(r `ith` i)]) t
ithCol i Empty = Nothing

numRows :: Table a -> Int
numRows Empty = 0
numRows t@(ConsT r rs) = numRows' t 0
  where
    numRows' (ConsT r rs) acc = numRows' rs (acc + 1)
    numRows' Empty acc = acc
  
numCols :: Table a -> Int
numCols Empty = 0
numCols t = case headTbl t of ConsT (Row r) _ -> length r
                              otherwise -> 0
  
-- Traverse through each row
modifyRows :: (Row a -> Row b) -> Table a -> Table b
modifyRows f (ConsT r rs) = ConsT (f r) (modifyRows f rs)
modifyRows _ Empty = Empty
  
filterRows :: (Row a -> Bool) -> Table a -> Table a
filterRows f (ConsT r rs)
  | f r = ConsT r (filterRows f rs)
  | otherwise = filterRows f rs
filterRows _ Empty = Empty
  
filterT :: (a -> Bool) -> Table a -> Table a
filterT f (ConsT (Row r) rs)
  | (length $ filter f r) /= 0 = eraseEmptyRows $ ConsT (Row r) (filterT f rs)
  | otherwise = filterT f rs
filterT f Empty = Empty

-- Miscellaneous --
fromCSV :: FilePath -> IO (Table Field)
fromCSV p = 
  (parseCSVFromFile p) >>= (\csv -> return $ either (\err -> Empty) (\c -> mkTable c) csv) 

toCSV :: Show a => String -> Table a -> IO ()
toCSV path t = writeFile path $ unlines $ toListOfStrings t ","

toTableOfType :: Table a -> (a -> b) -> Table b
toTableOfType t f = fmap f t

toListOfLists :: Table a -> [[a]]
toListOfLists (ConsT r rs) = (toList r) : (toListOfLists rs)
toListOfLists Empty = [[]]

toListOfStrings :: Show a => Table a -> String -> [String]
toListOfStrings (ConsT r rs) sep = (toString r sep) : (toListOfStrings rs sep)
toListOfStrings Empty _ = []

fromSQL :: IConnection c => c -> String -> [SqlValue] -> IO (Table SqlValue)
fromSQL conn query sqlValue = liftM mkTable (quickQuery conn query sqlValue)

toSQL :: (IConnection t, Show a) => Table a -> t -> String -> IO ()
toSQL t conn query = 
  let 
    listOfStrings = map (\str -> map toSql (words str)) (toListOfStrings t " ")
    insertRow = run conn query
  in 
    (forM listOfStrings insertRow) >> (commit conn) >> (disconnect conn)

grepT :: (RegexMaker Regex CompOption ExecOption pat, RegexLike Regex a) => 
          Table a -> pat -> Table a
grepT t pattern = filterT (\x -> x =~ pattern :: Bool) t

replaceSymbols :: Table [Char] -> Char -> Char -> Table [Char]
replaceSymbols t s1 s2 = fmap (\x -> map repl x) t
    where 
      repl c 
        | c == s1 = s2
        | otherwise = c