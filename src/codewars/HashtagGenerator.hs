-- https://www.codewars.com/kata/52449b062fb80683ec000024
module Codewars.Kata.Hashtag where

import           Control.Monad (liftM2, mfilter)
import           Data.Char     (toUpper)

generateHashtag :: String -> Maybe String
generateHashtag = mfilter (liftM2 (&&) (1<) (< 140) . length) . Just . ('#' :) . concatMap capitalizeFirst . words
  where
    capitalizeFirst (c:cs) = toUpper c : cs
