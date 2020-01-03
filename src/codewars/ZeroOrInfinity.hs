-- https://www.codewars.com/kata/55a29405bc7d2efaff00007c
module Codewars.Kata.Suite1 where

import           Data.List (tails)

going :: Integer -> Double
going n = toPrecision . sum . map (product . map ((1.0 /) . fromInteger)) . tails $ [2 .. n]
  where
    toPrecision :: Double -> Double
    toPrecision d = fromInteger (floor $ d * (10 ^ 6)) / (10.0 ^^ 6)
