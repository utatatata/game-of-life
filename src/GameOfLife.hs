import System.Environment (getArgs)
import Graphics.Gloss (Color(..), Display(..), Picture(..), blue, dark, red, simulate, white)
import Graphics.Gloss.Data.ViewPort (ViewPort)

-- Util

onn :: (b -> b -> b -> c) -> (a -> b) -> (a -> a -> a -> c)
onn f g x y z = f (g x) (g y) (g z)

trio :: [a] -> [(a, a, a)]
trio (x:y:z:ws) = (x, y, z) : (trio (y:z:ws))
trio _ = []

extend :: [a] -> [a]
extend [] = []
extend xs = last xs : xs ++ [head xs]

-- Data

data Cell
  = Alive
  | Dead
  deriving (Show, Eq)

fromChar :: Char -> Cell
fromChar '#' = Alive
fromChar _ = Dead

type State = [[Cell]]

fromLines :: [String] -> State
fromLines lines =
  [ [ fromChar chr
    | chr <- line
    ]
  | line <- lines
  ]

type Neighbor =
  ( (Cell, Cell, Cell)
  , (Cell, Cell, Cell)
  , (Cell, Cell, Cell)
  )

neighbors :: State -> [[Neighbor]]
neighbors css =
  let
    extended = extend $ map extend css
  in
    [ (zip3 `onn` trio) top middle bottom
    | (top, middle, bottom) <- trio extended
    ]

nextCell :: Neighbor -> Cell
nextCell
  ( (tl, tc, tr)
  , (ml, mc, mr)
  , (bl, bc, br)
  ) =
    let
      ns = length $ filter (== Alive) [tl, tc, tr, ml, mr, bl, bc, br]
    in
      case (mc, ns) of
        (Dead, 3) -> Alive
        (Alive, ns) | ns == 2 || ns == 3 -> Alive
        _ -> Dead

-- GUI

size :: Float
size = 45

offset :: Float
offset = 5

cell :: Int -> Int -> Color -> Picture
cell posX posY color =
  let
    width = size + offset
    [x, y] = map ((* width) . fromIntegral) [posX, posY]
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

draw :: State -> Picture
draw css =
  Pictures
    [ cell x y $ cellColor c
    | (y, cs) <- zip [0..] css
    , (x, c) <- zip [0..] cs
    ]

nextState :: ViewPort -> Float -> State -> State
nextState _ _ css = map (map nextCell) $ neighbors css

window :: Display
window = InWindow "Game of Life" (500, 500) (100, 100)

fps :: Int
fps = 2

main :: IO ()
main = do
  filePath : _  <- getArgs
  text <- readFile filePath
  print $ fromLines $ lines text
  simulate window (dark white) fps (fromLines $ lines text) draw nextState
