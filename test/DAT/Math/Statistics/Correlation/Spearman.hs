module DAT.Math.Statistics.Correlation.Spearman (spec) where

import Test.Hspec
import DAT.Row
import DAT.Table
import DAT.Math.Statistics.Correlation.Spearman

spec :: Spec
spec = do
  describe "Spearman" $ do
    it "should compute Spearman correlation" $ do
      spearman t `shouldBe` 0.6428571428571428