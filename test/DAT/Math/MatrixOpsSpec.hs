module DAT.Math.MatrixOpsSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Table
import DAT.Math.MatrixOps

spec :: Spec
spec = do
  describe "Matrix Operations" $ do
    it "should correctly compute 3x3 matrix determinant" $ do
      determinant t1 `shouldBe` 0
    it "should correctly compute 4x4 matrix determinant" $ do
      determinant t2 `shouldBe` (-1436)
    it "should correctly compute 5x5 matrix determinant" $ do
      determinant t3 `shouldBe` 24744

t1 :: Table Double
t1 = ConsT (Row [1, 2, 3]) (ConsT (Row [4, 5, 6]) (ConsT (Row [7, 8, 9]) Empty))

t2 :: Table Double
t2 = ConsT (Row [1, 2, 3, 4]) (ConsT (Row [4, 4, 5, 11]) (ConsT (Row [6, 1, 12, 23]) (ConsT (Row [0, 6, 66, 88]) Empty)))

t3 :: Table Double
t3 = ConsT (Row [2, 3, 4, 5, 6]) (ConsT (Row [1, 2, 3, 4, 4]) (ConsT (Row [1, 2, 3, 2, 3]) (ConsT (Row [3, 45, 6, 6, 3]) (ConsT (Row [1, 22, 12, 323, 11]) Empty))))
