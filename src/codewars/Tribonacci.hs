-- https://www.codewars.com/kata/556deca17c58da83c00002db
module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n $ tribonacciSeq a b c

tribonacciSeq :: Num a => a -> a -> a -> [a]
tribonacciSeq a b c = a : tribonacciSeq b c (a + b + c)
