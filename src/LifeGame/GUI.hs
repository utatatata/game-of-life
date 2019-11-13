module LifeGame.GUI where

-- import Graphics.Gloss (Color(..), Picture(..))
import Graphics.Gloss

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

