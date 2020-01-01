-- https://www.codewars.com/kata/54ff3102c1bad923760001f3
module Codewars.Kata.VowelCount where

getCount :: String -> Int
getCount = length . filter isVowel
  where isVowel = flip elem "aeiou" 