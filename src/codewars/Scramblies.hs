-- https://www.codewars.com/kata/55c04b4cc56a697bb0000048
module Codewars.G964.Scramblies where

import Data.List ((\\))

scramble :: String -> String -> Bool
scramble s1 s2 = null (s2 \\ s1) 