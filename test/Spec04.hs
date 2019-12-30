module Spec04
  ( spec
  ) where

import Test.Hspec
import CodeKata04

spec :: IO ()
spec = hspec $ do
  describe "spread" $
    it "Should return the difference between max and min temps" $ do
      spread (Row "1" 10 4) `shouldBe` 6
      spread (Row "®" 20 35) `shouldBe` 15

  describe "minSpread" $ do
    let w1 = Row "2" 20 10
    let w2 = Row "3" 21 15
    let w3 = Row "4" 11 9
    it "Should return the Weather with the minimum spread" $ do
      minSpread [w1, w2, w3] `shouldBe` w3
      minSpread [w3, w2, w1] `shouldBe` w3
      minSpread [w2, w3, w1] `shouldBe` w3

  describe "parseWeather" $ do
    it "Should parse a Row from a sound weather string" $ do
      parseWeather " 1 88 59 asdkasdsad" `shouldBe` Just (Row "1" 88 59)
      parseWeather " ci 88 59 asdkasdsad" `shouldBe` Just (Row "ci" 88 59)
    it "Should return Nothing for a wrong string" $
      parseWeather " 1 23 asd asd" `shouldBe` Nothing
      
    describe "parseFootball" $ do
      it "Should parse a Row from a sound football string" $
        parseFootball " 1. Arsenal 38    26   9   3    79  -  36 87" `shouldBe` Just (Row "Arsenal" 79 36)
      it "Should return Nothing for a wrong string" $
        parseFootball " 1. Arsenal 23 asd asd" `shouldBe` Nothing
