{-# LANGUAGE FlexibleContexts #-} 

module DAT.Table.Misc where

import DAT.Row
import DAT.Table
import Text.CSV
import Control.Monad
import Database.HDBC
import Data.List
import Text.Regex.Posix 
import Text.Regex.Posix.Wrap 
import Text.Regex.Base.RegexLike

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

-- Find rows which value in column with specified index matches with a 
-- given value
matchingRowsByCol :: Eq a => Table a -> a -> Int -> Table a
matchingRowsByCol = matchingRowsByCol' Empty
  where
    matchingRowsByCol' acc t@(ConsT r rs) value id
      | r `ith` id == value = 
          matchingRowsByCol' (appendT acc (toTable r)) rs value id
      | otherwise = matchingRowsByCol' acc rs value id
    matchingRowsByCol' acc Empty _ _ = acc

findNotUniqueByCol :: (Eq a, Ord a) => Table a -> Int -> Table a
findNotUniqueByCol Empty _ = Empty
findNotUniqueByCol t id = findNotUniqueByCol' Empty (sortT id t) Nothing
  where
    findNotUniqueByCol' acc@(ConsT r1 _) (ConsT r rs) prev
      | lookUp (r `ith` id) prev && not (lookUp (r1 `ith` id) prev) = 
          findNotUniqueByCol' (prependT acc (toTable r)) rs prev
      | otherwise = findNotUniqueByCol' acc rs (Just (r `ith` id))
    findNotUniqueByCol' Empty (ConsT r rs) prev  
      | lookUp (r `ith` id) prev = findNotUniqueByCol' (prependT Empty (toTable r)) rs prev
      | otherwise = findNotUniqueByCol' Empty rs (Just (r `ith` id))
    findNotUniqueByCol' acc Empty _ = acc

    lookUp x (Just y) = x == y
    lookUp _ Nothing = False