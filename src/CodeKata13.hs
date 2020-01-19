module CodeKata13
  ( codeLineCount
  ) where

import           Data.Char

-- //
-- /*
-- anything
-- */
isLineComment :: String -> Bool
isLineComment s =
  case trimStart s of
    ('/':'/':_) -> True
    _           -> False

trimStart :: String -> String
trimStart = dropWhile isSpace

codeLineCount :: [String] -> Int
codeLineCount = length . codeLines

codeLines :: [String] -> [String]
codeLines = removeBlankLines . removeComments

removeComments :: [String] -> [String]
removeComments = removeSingleLine . removeMultiLine

removeMultiLine [] = []
removeMultiLine (s:ss) =
  case trimStart s of
    ('/':'*':cs) -> removeMultiLine . processCommentEnd . dropWhile (not . endsComment) $ (cs : ss)
    _ -> s : removeMultiLine ss

endsComment :: String -> Bool
endsComment []         = False
endsComment (c1:c2:cs) = ([c1, c2] == "*/") || endsComment (c2 : cs)
endsComment (c:cs)     = endsComment cs

processCommentEnd :: [String] -> [String]
processCommentEnd (s:ss) = dropUntilCommentEnd s : ss
  where
    dropUntilCommentEnd (c1:c2:cs) =
      if [c1, c2] == "*/"
        then cs
        else dropUntilCommentEnd (c2 : cs)

removeSingleLine :: [String] -> [String]
removeSingleLine = filter (not . isLineComment)

removeBlankLines :: [String] -> [String]
removeBlankLines = filter (not . all isSpace)
