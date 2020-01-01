-- https://www.codewars.com/kata/530e15517bc88ac656000716
module Rot13 where

import           Data.Char

rot13 :: String -> String
rot13 = map transform
  where
    transform c
      | not $ isAscii c = c
      | isUpper c = go (ord 'A') c
      | isLower c = go (ord 'a') c
      | otherwise = c
    go a = chr . (+ a) . (`mod` 26) . (+ 13) . flip (-) a . ord
