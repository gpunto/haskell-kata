-- https://www.codewars.com/kata/54da5a58ea159efa38000836
module Codewars.Kata.FindOdd where

import           Data.List (group, sort)

findOdd :: [Int] -> Int
findOdd = head . head . filter (odd . length) . group . sort
