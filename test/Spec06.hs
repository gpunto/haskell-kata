module Spec06
  ( spec
  ) where

import           CodeKata06
import           Test.Hspec

spec :: IO ()
spec =
  hspec $
    describe "solution" $
      it "Should return grouped anagrams" $ do
        let w1 = "Ab'C"
        let w2 = "acc"
        let w3 = "iP-o"
        let w4 = "cca"
        let w5 = "poi"
        solution [w1, w2, w3, w4, w5] `shouldBe` [[w1], [w2, w4], [w3, w5]]
