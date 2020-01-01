module Narcissistic where

narcissistic :: Integral n => n -> Bool
narcissistic n = (n ==) . sum . map (^ e) $ ds
  where
    ds = digits [] n
    e = length ds

digits :: Integral n => [n] -> n -> [n]
digits acc 0 = acc
digits acc n = digits (mod n 10 : acc) (div n 10)
