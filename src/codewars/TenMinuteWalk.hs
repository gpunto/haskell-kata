-- https://www.codewars.com/kata/54da539698b8a2ad76000228
module Codewars.Kata.TenMinuteWalk where

type Direction = Char

data Distance =
  Distance
    { vertical   :: Int
    , horizontal :: Int
    }
  deriving (Eq)

isValidWalk :: [Direction] -> Bool
isValidWalk walk = is10Minutes walk && goesBack walk
  where
    is10Minutes = (== 10) . length . take 11
    goesBack = (Distance 0 0 ==) . foldl addDist (Distance 0 0)
    addDist acc dir =
      case dir of
        'n' -> acc {vertical = vertical acc + 1}
        's' -> acc {vertical = vertical acc - 1}
        'w' -> acc {horizontal = horizontal acc - 1}
        'e' -> acc {horizontal = horizontal acc + 1}
