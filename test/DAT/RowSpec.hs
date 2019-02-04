module DAT.RowSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Math.Vector

spec :: Spec
spec = do
  describe "Row" $ do
    it "..." $ do
      size (Row [1, 2]) `shouldBe` 2