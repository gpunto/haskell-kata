module CodeKata06
  ( solution
  ) where

import           Data.Char
import           Data.List
import qualified Data.List.Extra as Ex

normalize :: String -> String
normalize = sort . map toLower . filter isLetter

solution :: [String] -> [[String]]
solution = Ex.groupSortOn normalize
--solution = M.elems . L.fold (L.groupBy normalize L.list)
