-- https://www.codewars.com/kata/55e7280b40e1c4a06d0000aa
module Codewars.G964.SumOfK where

import           Control.Monad (mfilter)
import           Data.List     (group, permutations, sort, subsequences)

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum maxDistance nTowns ls = solve =<< mfilter ((nTowns <=) . length) (Just ls)
  where
    solve = safeMax . filter (<= maxDistance) . map sum . subsequencesOfSize nTowns
    safeMax [] = Nothing
    safeMax xs = Just $ maximum xs

-- subsequencesOfSize copied from:
-- https://stackoverflow.com/questions/21265454/subsequences-of-length-n-from-list-performance/21288092#21288092
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l
        then []
        else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x:xs) =
      let next = subsequencesBySize xs
       in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])
