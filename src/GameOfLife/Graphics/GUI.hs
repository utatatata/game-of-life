module GameOfLife.Graphics.GUI
  (run
  )
  where

import qualified Data.DoubleZipper as DZ
import Data.Function (on)
import GameOfLife.Field (Cell(..), Field)
import qualified GameOfLife.Field as F
import Graphics.Gloss (Color(..), Display(..), Picture(..), blue, dark, red, simulate, white)
import Graphics.Gloss.Data.ViewPort (ViewPort)

cellSize :: Float
cellSize = 45

cellMargin :: Float
cellMargin = 5

wrapperSize :: Float
wrapperSize = cellSize + cellMargin

cell :: Int -> Int -> Color -> Picture
cell posX posY color =
  Color color
    $ Polygon
      [ (x, y)
      , (x + cellSize, y)
      , (x + cellSize, y + cellSize)
      , (x, y + cellSize)
      ]
  where
  [x, y] = map ((* wrapperSize) . fromIntegral) [posX, posY]

cellColor :: Cell -> Color
cellColor Alive = red
cellColor Dead = blue

draw :: Int -> Int -> Field -> Picture
draw width height f =
  Pictures
    [ cell x y $ cellColor c
    | (y, cs) <- zip [0..] css
    , (x, c) <- zip [0..] cs
    ]
  where
  css = map (take width) $ take height $ DZ.toList f

nextState :: ViewPort -> Float -> Field -> Field
nextState _ _ = F.update

window :: Int -> Int -> Display
window width height =
  InWindow "GameOfLife" size pos
  where
  ws = round wrapperSize
  size = ((,) `on` (* ws)) width height
  pos = (100, 100)


run :: (Int, Int) -> Int -> Field -> IO ()
run (width, height) fps initField =
  simulate (window width height) (dark white) fps initField (draw width height) nextState
