module Kata
  ( findOutlier
  ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)

findOutlier :: [Int] -> Int
findOutlier xs = fromJust $ find outlier xs
  where
    evenNo = length . filter even $ take 3 xs
    outlier =
      if evenNo > 1
        then odd
        else even
