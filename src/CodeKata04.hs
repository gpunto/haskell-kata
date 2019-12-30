module CodeKata04
  ( Row(..)
  , spread
  , minSpread
  , parseWeather
  , parseFootball
  , solution1
  , solution2
  ) where

import           Data.Foldable (minimumBy)
import           Data.Maybe    (mapMaybe)
import           Data.Ord      (comparing)
import           Text.Read     (readMaybe)

data Row =
  Row
    { _id :: String
    , hi  :: Float
    , lo  :: Float
    }
  deriving (Eq, Show)

spread :: Row -> Float
spread w = abs $ hi w - lo w

minSpread :: [Row] -> Row
minSpread = minimumBy $ comparing spread

parseWeather :: String -> Maybe Row
parseWeather s =
  case take 3 . words $ s of
    [_id, max, min] -> parseRow (_id, max, min)
    _               -> Nothing

parseFootball :: String -> Maybe Row
parseFootball s =
  case words s of
    [_, name, _, _, _, _, g, _, a, _] -> parseRow (name, g, a)
    _                                 -> Nothing

parseRow :: (String, String, String) -> Maybe Row
parseRow (i, h, l) =
  let hM = readMaybe h :: Maybe Float
      lM = readMaybe l :: Maybe Float
   in case (hM, lM) of
        (Just hi, Just lo) -> Just (Row i hi lo)
        _                  -> Nothing

solution1 :: String -> String
solution1 = solution parseWeather

solution2 :: String -> String
solution2 = solution parseFootball

solution :: (String -> Maybe Row) -> String -> String
solution f = _id . minSpread . mapMaybe f . lines
