module Codewars.Kata.Compare where

import           Data.List (sort)

comp :: [Integer] -> [Integer] -> Bool
comp as bs = sort bs == (sort . map (^ 2)) as
