module DAT.Math.Statistics.Correlation.SpearmanSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Table
import DAT.Math.Statistics.Correlation.Spearman

spec :: Spec
spec = do
  describe "Spearman" $ do
    it "should compute Spearman correlation" $ do
      spearman r1 r2 `shouldBe` 0.6428571428571428

r1 :: Row Double
r1 = Row [79, 76, 78, 65, 86, 82, 91]

r2 :: Row Double
r2 = Row [77, 78, 79, 80, 86, 89, 91]