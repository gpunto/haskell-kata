-- https://www.codewars.com/kata/52774a314c2333f0a7000688
module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses = balanced 0 . filter (`elem` "()")
  where
    balanced count []       = count == 0
    balanced 0 (')':ps)     = False
    balanced count ('(':ps) = balanced (count + 1) ps
    balanced count (')':ps) = balanced (count - 1) ps
