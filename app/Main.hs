module Main where

import Control.Comonad (Comonad(..))
import Data.DoubleZipper (DoubleZipper(..))
import Data.Zipper (Zipper(..))
import Data.List (intercalate)

import qualified Data.DoubleZipper as DZ
import qualified GameOfLife.CLI as CLI
import GameOfLife.Field (Field, Cell(..))
import qualified GameOfLife.Field as F
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
  CLI.run (width, height) 10 field
