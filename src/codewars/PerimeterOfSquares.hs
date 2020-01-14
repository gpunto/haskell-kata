-- https://www.codewars.com/kata/559a28007caad2ac4e000083
module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter n = 4 * go 1 1 2 (n - 1)
  where
    go _ _ acc 0 = acc
    go n2 n1 acc count = go n1 n0 (acc + n0) (count - 1)
      where n0 = n1 + n2