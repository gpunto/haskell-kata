module AllProducts where

getProducts :: [Integer] -> [Integer] -> [Integer]
getProducts xs ys = [x * y | y <- ys, x <- xs ]