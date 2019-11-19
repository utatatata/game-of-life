module GameOfLife.CLI
  ( run
  )
  where

import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Data.List (intercalate)
import qualified Data.DoubleZipper as DZ
import GameOfLife.Field (Field(..))
import qualified GameOfLife.Field as F

draw :: Int -> Int -> Field -> IO ()
draw width height f =
  let
    css = DZ.toList f
  in
    flip mapM_ (take height css) $ \cs ->
      putStrLn $ intercalate " " $ map (pure . F.toChar) $ take width cs

run :: (Int, Int) -> Int -> Field -> IO ()
run (width, height) fps initField =
  loop initField
  where
  loop field = do
    replicateM_ height $ putStr "\ESC[A\ESC[2K" -- move the cursor up and clear entire line
    draw width height field
    let field' = F.update field
    threadDelay $ 1000000 `div` fps
    loop field'
