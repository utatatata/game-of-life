module GameOfLife.Field
  ( Cell(..)
  , fromChar
  , toChar
  , Field
  , fromStrings
  , update
  )
  where

import Control.Comonad (Comonad(..))
import Data.Zipper (Zipper(..))
import Data.DoubleZipper (DoubleZipper(..))
import qualified Data.DoubleZipper as DZ

data Cell
  = Alive
  | Dead
  deriving (Show, Eq)

fromChar :: Char -> Cell
fromChar '#' = Alive
fromChar _ = Dead

toChar :: Cell -> Char
toChar Alive = '#'
toChar Dead = '.'

type Field = DoubleZipper Cell

fromStrings :: [String] -> Field
fromStrings css =
  DZ.fromList Dead $ map (map fromChar) css

update :: Field -> Field
update f = extend nextCell f
  where
  nextCell :: Field -> Cell
  nextCell (DoubleZipper (Zipper
      (Zipper (tl:_) tc (tr:_):_)
      (Zipper (cl:_) cc (cr:_))
      (Zipper (bl:_) bc (br:_):_)
    )) =
      let
        neightbors = length $ filter (== Alive) [ tl, tc, tr, cl, cr, bl, bc, br ]
      in
        case (cc, neightbors) of
          (Dead, 3) -> Alive
          (Alive, 2) -> Alive
          (Alive, 3) -> Alive
          _ -> Dead
  