-- https://www.codewars.com/kata/523a86aa4230ebb5420001e1
module Anagram where

import           Data.List (sort)

anagrams :: String -> [String] -> [String]
anagrams w = filter ((s ==) . sort)
  where
    s = sort w
