module DAT.RowSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Math.Vector

spec :: Spec
spec = do
  describe "Row" $ do
    it "should join list of rows into a single Row" $ do
      joinR [Row [1], Row [2]] `shouldBe` (Row [1, 2])
    
    it "should concatenate two rows into a single Row" $ do
      concatR (Row [1]) (Row [2]) `shouldBe` (Row [1, 2])
    
    it "should correctly filter a Row" $ do
      filterR (\x -> x == 1) (Row [1, 2, 1, 2]) `shouldBe` (Row [1, 1])
    
    it "should zip two Rows with a given binary function and yield another Row" $ do
        zipWithR (\x  y -> x + y) (Row [1, 1]) (Row [2, 2]) `shouldBe` (Row [3, 3])
    
    it "should zip two Rows and yield another Row with 2-tupled type" $ do
      zipR (Row [1, 2]) (Row [3, 4]) `shouldBe` (Row [(1, 3), (2, 4)])
    
    it "should yield first element (head) of Row which is also a Row" $ do
      headR (Row [1, 2]) `shouldBe` (Row [1])
    
    it "should yield last elements excluding first (tail) of Row which is also a Row" $ do
      tailR (Row [1, 2]) `shouldBe` (Row [2])
    
    it "should return the i-th element of a Row by the given index" $ do
      ((Row [1, 2, 3]) `ith` 1) `shouldBe` 2
    
    it "should return Row length" $  do
      length (Row [1, 2, 3]) `shouldBe` 3

    it "should erase i-th element of a given Row and yield result Row" $ do
      eraseIth 1 (Row [1, 2, 3]) `shouldBe` (Row [1, 3])

    it "should erase a given Rrow to the given element index and yield result Row" $  do
      splice 1 (Row [1, 2, 3]) `shouldBe` (Row [3])

    it "should convert given Row to another type" $ do
      toRowOfType (Row [1, 2, 3]) (\x -> show x) `shouldBe` (Row ["1", "2", "3"]) 
    
    it "should convert given Row to a strting separated with a given symbol" $ do
      toString (Row [1, 2, 3]) " " `shouldBe` "1 2 3"
  
    it "should convert a given Row to a List" $ do
      toList (Row [1, 2, 3]) `shouldBe` [1, 2, 3]