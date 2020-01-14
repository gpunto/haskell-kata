-- https://www.codewars.com/kata/54d7660d2daf68c619000d95
module Codewars.Kata.CommonDenominators where

import           Data.Function (on)

type Ratio a = (a, a) -- Data.Ratio not suitable for this kata

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs xs = map (\(a, b) -> (den * a `div` b, den)) xs
  where
    den = foldr1 lcm . map snd $ xs
