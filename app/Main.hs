module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  file : _ <- getArgs
  text <- readFile file
  print $ lines text
