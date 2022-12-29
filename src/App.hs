module App
  ( app
  ) where

import Control.Monad.State
import System.IO

import Types
import Board
import GetKey

app :: IO ()
app = do
  hSetBuffering stdin NoBuffering
  board <- execStateT initBoard emptyBoard
  showBoard board
  play board

play :: Board -> IO()
play board = do
  key <- getKey
  case key of
    QuitKey -> return ()
    UnknownKey -> play board
    _ -> do
      showBoard board
      play board
