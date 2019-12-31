module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount = length . filter isVowel
  where isVowel = flip elem "aeiou" 