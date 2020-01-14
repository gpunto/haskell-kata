-- https://www.codewars.com/kata/5547cc7dcad755e480000004
module Codewars.Kata.RemovNB where

removNb :: Integer -> [(Integer, Integer)]
removNb n = [(a, b) | a <- [1 .. n], let (b, r) = f a, r == 0 && b < n]
  where
    total = n * (n + 1) `div` 2
    f a = (total - a) `divMod` (a + 1)
