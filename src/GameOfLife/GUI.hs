module GameOfLife.GUI where

import GameOfLife.Field (Cell(..))
import Graphics.Gloss (Color(..), Display(..), Picture(..), blue, dark, red, simulate, white)
import Graphics.Gloss.Data.ViewPort (ViewPort)

size :: Float
size = 45

offset :: Float
offset = 5

cell :: Int -> Int -> Color -> Picture
cell posX posY color =
  let
    width = size + offset
    [x, y] = map ((* width) . fromIntegral) $ [posX, posY]
  in
    Color color
      $ Polygon
        [ (x, y)
        , (x + size, y)
        , (x + size, y + size)
        , (x, y + size)
        ]

cellColor :: Cell -> Color
cellColor Alive = red
cellColor Dead = blue

-- draw :: Field -> Picture
-- draw f =
--   Pictures
--     []
