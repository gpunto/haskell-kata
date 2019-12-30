module Spec09
  ( spec
  ) where

import           CodeKata09
import qualified Data.Map   as Map
import           Test.Hspec

spec :: IO ()
spec =
  hspec $ do
    describe "toCart" $
      it "Should return a map from items to quantity" $
      toCart ['A', 'B', 'C', 'A', 'A', 'B', 'D', 'C'] `shouldBe` Map.fromList [('A', 3), ('B', 2), ('C', 2), ('D', 1)]
    describe "totalPrice" $
      it "Should return the correct total price" $ do
        let rules =
              Map.fromList
                [('A', [Rule 1 50, Rule 3 130]), ('B', [Rule 1 30, Rule 2 45]), ('C', [Rule 1 20]), ('D', [Rule 1 15])]
        totalPrice rules "" `shouldBe` 0
        totalPrice rules "A" `shouldBe` 50
        totalPrice rules "AB" `shouldBe` 80
        totalPrice rules "CDBA" `shouldBe` 115
        totalPrice rules "AA" `shouldBe` 100
        totalPrice rules "AAA" `shouldBe` 130
        totalPrice rules "AAAA" `shouldBe` 180
        totalPrice rules "AAAAA" `shouldBe` 230
        totalPrice rules "AAAAAA" `shouldBe` 260
        totalPrice rules "AAAB" `shouldBe` 160
        totalPrice rules "AAABB" `shouldBe` 175
        totalPrice rules "AAABBD" `shouldBe` 190
        totalPrice rules "DABABA" `shouldBe` 190
