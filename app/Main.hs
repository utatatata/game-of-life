module Main where

import qualified GameOfLife.CLI as CLI
import qualified GameOfLife.Field as F
import qualified GameOfLife.GUI as GUI
import System.Environment (getArgs)

main :: IO ()
main = do
  file : _ <- getArgs
  text <- lines <$> readFile file
  let
    width = case text of
      xs@(_:_) : _ -> length xs
      _ -> 0
    height = case text of
      xs@(_ : _) -> length xs
      [] -> 0
    field = F.fromStrings text
  -- CLI.run (width, height) 10 field
  GUI.run (width, height) 3 field
