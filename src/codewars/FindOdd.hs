module Codewars.Kata.FindOdd where

import           Data.List (group, sort)

findOdd :: [Int] -> Int
findOdd = head . head . filter (odd . length) . group . sort
