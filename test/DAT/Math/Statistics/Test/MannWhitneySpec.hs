module DAT.Math.Statistics.Test.MannWhitneySpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Table
import DAT.Math.Statistics.Test.MannWhitney

spec :: Spec
spec = do
  describe "MannWhitney" $ do
    it "should compute Mann-Whitney test" $ do
      mannWhitney t `shouldBe` MannWhitneyRes {n1 = 6, n2 = 6, u = 1.0}

t :: Table Double
t = ConsT (Row [3, 4, 2, 6, 2, 5]) (ConsT (Row [9, 7, 5, 10, 6, 8]) Empty)