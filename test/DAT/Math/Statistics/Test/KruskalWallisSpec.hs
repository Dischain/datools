module DAT.Math.Statistics.Test.KruskalWallisSpec (spec) where

import Test.Hspec
import DAT.Row
import DAT.Table
import DAT.Math.Statistics.Test.KruskalWallis

spec :: Spec
spec = do
  describe "KruskalWallis" $ do
    it "should compute Kruskal-Wallis test" $ do
      kruskallWallis t `shouldBe` 
        KruskalWallisRes {h = 1.824561403508767, ndf = 2}

t :: Table Double
t = ConsT (Row [8.2, 10.3, 9.1, 12.6, 11.4, 13.2]) (ConsT (Row [10.2, 9.1, 13.9, 14.5, 9.1, 16.4]) (ConsT (Row [13.5, 8.4, 9.6, 13.8, 17.4, 15.3]) Empty))