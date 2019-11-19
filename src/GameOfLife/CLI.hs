module GameOfLife.CLI
  (run
  )
  where

import Data.Semigroup ((<>))
import qualified GameOfLife.Graphics.CLI as CLI
import qualified GameOfLife.Field as F
import qualified GameOfLife.Graphics.GUI as GUI
import Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, short, strArgument, switch, (<**>))
import System.Environment (getArgs)

data Args = Args
  { file :: String
  , inCLI :: Bool
  , inGUI :: Bool
  }

data Mode
  = CLI
  | GUI

args :: Parser Args
args = Args
  <$> strArgument
      ( metavar "FILE"
      <> help "Path to a file giving an initial state of cells."
      )
  <*> switch
      ( long "cli"
      <> short 'c'
      <> help "Run in CLI."
      )
  <*> switch
      ( long "gui"
      <> short 'g'
      <> help "Run in GUI (use GLUT)."
      )

command :: Args -> IO ()
command (Args file inCLI inGUI) = do
  text <- lines <$> readFile file
  let
    width = case text of
      xs@(_:_) : _ -> length xs
      _ -> 0
    height = case text of
      xs@(_ : _) -> length xs
      [] -> 0
    field = F.fromStrings text
  case (inCLI, inGUI) of
    (_, True) -> GUI.run (width, height) 3 field
    _ -> CLI.run (width, height) 10 field

run :: IO ()
run = command =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
      <> header "life - Game of Life"
      )
