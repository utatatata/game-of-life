module LifeGame.Field
  ( State(..)
  , Field
  , nextState
  )
  where

import Control.Comonad (Comonad(..))
import Data.Zipper (Zipper(..))
import Data.DoubleZipper (DoubleZipper(..))

data State
  = Alive
  | Dead
  deriving (Show, Eq)

newtype Field = Field (DoubleZipper State)

nextState :: Field -> State
nextState (Field (DoubleZipper (Zipper
    (Zipper (tl:_) tc (tr:_):_)
    (Zipper (cl:_) cc (cr:_))
    (Zipper (bl:_) bc (br:_):_)
  ))) =
    let
      neightbors = length $ filter (== Alive) [ tl, tc, tr, cl, cr, bl, bc, br ]
    in
      case (cc, neightbors) of
        (Dead, n) | n == 3 -> Alive
        (Alive, n) | n == 2 || n == 3 -> Alive
        (state, n) -> state

