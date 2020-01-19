module Main where

import qualified CodeKata04
import qualified CodeKata06
import qualified CodeKata13

main :: IO ()
main = codeKata13

codeKata04 = do
  putStrLn . CodeKata04.solution1 =<< readFile "weather.dat"
  putStrLn . CodeKata04.solution2 =<< readFile "football.dat"

codeKata06 = solveAndShow <$> readFile "wordlist.txt" >>= writeFile "anagrams.txt"
  where
    solveAndShow = unlines . map show . CodeKata06.solution . lines

codeKata13 = print . CodeKata13.codeLineCount . lines =<< readFile "Hello.java"

