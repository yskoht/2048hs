module App
  ( app
  ) where

import Control.Monad.State
import System.IO

import Types
import Board

app :: IO ()
app = do
  hSetBuffering stdin NoBuffering
  board <- execStateT initBoard emptyBoard
  play board

play :: Board -> IO()
play board = do
  c <- getChar
  showBoard board
  case c of
    'q' -> return ()
    _ -> play board
