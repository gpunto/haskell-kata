-- https://www.codewars.com/kata/550f22f4d758534c1100025a
module Codewars.Kata.Reduction where

--import Codewars.Kata.Reduction.Direction

data Direction
  = North
  | East
  | West
  | South
  deriving (Eq, Show)

dirReduce :: [Direction] -> [Direction]
dirReduce ds =
  if res == ds
    then res
    else dirReduce res
  where
    res = dirReduceCo [] ds

dirReduceCo :: [Direction] -> [Direction] -> [Direction]
dirReduceCo acc [] = reverse acc
dirReduceCo acc [d] = dirReduceCo (d : acc) []
dirReduceCo acc (d1:d2:ds) =
  if elide d1 d2
    then dirReduceCo acc ds
    else dirReduceCo (d1 : acc) (d2 : ds)
  where
    elide North o = o == South
    elide South o = o == North
    elide West o  = o == East
    elide East o  = o == West
