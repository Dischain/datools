module DAT.Math.Statistics.Correlation.PearsonSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Math.Statistics.Correlation.Pearson

spec :: Spec
spec = do
  describe "Pearson" $ do
    it "should compute Pearson correlation" $ do
      pearson r1 r2 `shouldBe` 0.8846153846153847

r1 :: Row Double
r1 = Row [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]

r2 :: Row Double
r2 = Row [1, 2, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11]