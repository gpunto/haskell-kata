module Q1_10 where

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
-- 7
-- 8
-- 9
-- 10
