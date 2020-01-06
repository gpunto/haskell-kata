module Q1_10 where

import           Control.Monad (liftM2)
import           Data.List     (group)

-- 1
myLast :: [a] -> a
myLast [a]    = a
myLast (_:as) = myLast as

myLast' :: [a] -> a
myLast' = foldr1 (flip const)

-- 2
myButLast :: [a] -> a
myButLast [a, _] = a
myButLast (_:as) = myButLast as

-- 3
elementAt :: [a] -> Int -> a
elementAt ls at = go ls 1
  where
    go (x:xs) n
      | n == at = x
      | otherwise = go xs (n + 1)

elementAt' :: [a] -> Int -> a
elementAt' ls at = snd . head . filter ((== at) . fst) . zip [1 ..] $ ls

-- 4
myLength :: [a] -> Int
myLength = foldr ((+) . const 1) 0

myLength' :: [a] -> Int
myLength' [] = 0
myLength' xs = (1 +) . myLength' . tail $ xs

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverse' :: [a] -> [a]
myReverse' []     = []
myReverse' (a:as) = myReverse' as ++ [a]

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = reverse ls == ls

-- 7
data NestedList a
  = Elem a
  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a)  = [a]
flatten (List []) = []
flatten (List l)  = foldr1 (++) . map flatten $ l

-- 8
compress :: Eq a => [a] -> [a]
compress = map head . group

compress' :: Eq a => [a] -> [a]
compress' = foldr f []
  where
    f a [] = [a]
    f a xs =
      if head xs == a
        then xs
        else a : xs

-- 9
pack :: Eq a => [a] -> [[a]]
pack = group

pack' :: Eq a => [a] -> [[a]]
pack' = foldr f []
  where
    f a [] = [[a]]
    f a (xs:xxs) =
      if head xs == a
        then (a : xs) : xxs
        else [a] : xs : xxs

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (liftM2 (,) length head) . group
