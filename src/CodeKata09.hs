module CodeKata09
  ( toCart
  , totalPrice
  , itemPrice
  , Rule(..)
  ) where

import qualified Data.List as L
import qualified Data.Map  as M

type Item = Char

type Cart = M.Map Item Int

data Rule =
  Rule
    { quantity :: Int
    , price    :: Int
    }
  deriving (Eq, Ord, Show)

insert :: Item -> Cart -> Cart
insert item = M.insertWith (+) item 1

toCart :: [Item] -> Cart
toCart = foldr insert M.empty

totalPrice :: M.Map Item [Rule] -> [Item] -> Int
totalPrice ruleMap items = sum $ M.mapWithKey (priceWith ruleMap) (toCart items)

priceWith :: M.Map Item [Rule] -> Item -> Int -> Int
priceWith ruleMap item qty =
  case M.lookup item ruleMap of
    Just rs -> itemPrice (L.sortBy (flip compare) rs) 0 qty
    Nothing -> 0

itemPrice :: [Rule] -> Int -> Int -> Int
itemPrice _ acc 0 = acc
itemPrice (r:rs) acc qty = itemPrice rs (acc + p) rem
  where
    p = q * price r
    (q, rem) = qty `divMod` quantity r
