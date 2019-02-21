module DAT.Math.Statistics.Correlation.KendallSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Table
import DAT.Math.Statistics.Test.Rank hiding (r1)
import DAT.Math.Statistics.Correlation.Kendall

spec :: Spec
spec = do
  describe "Kendall" $ do
    it "should compute Kendall correlation" $ do
      kendall r1 r2 `shouldBe` 0.8484848484848485

r1 :: Row Rank
r1 = fmap (Rank) (Row [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])

r2 :: Row Rank
r2 = fmap (Rank) (Row [1, 2, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11])