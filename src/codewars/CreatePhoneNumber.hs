-- https://www.codewars.com/kata/525f50e3b73515a6db000b83
module CreatePhoneNumber where

import           Data.Char (intToDigit)

createPhoneNumber :: [Int] -> String
createPhoneNumber = reverse . foldl appendDigit "" . zip [0 ..]
  where
    appendDigit acc (index, num) =
      case index of
        0 -> n : '(' : acc
        2 -> ' ' : ')' : n : acc
        5 -> '-' : n : acc
        _ -> n : acc
      where
        n = intToDigit num
