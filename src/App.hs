module App
  ( app
  ) where

import Control.Monad.State
import System.IO
import System.Process

import Types
import Board
import Square
import GetKey

app :: IO ()
app = do
  hSetBuffering stdin NoBuffering
  board <- execStateT initBoard emptyBoard
  clear
  showBoard board
  play board

play :: Board -> IO()
play board = do
  key <- getKey
  case key of
    QuitKey -> return ()
    UnknownKey -> play board
    _ -> do
      let newBoard = move key board
      newBoard2 <- if newBoard /= board
                    then do
                      number <- createNumber
                      execStateT (addNumberS number) newBoard
                    else return newBoard
      clear
      showBoard newBoard2
      play newBoard2

clear :: IO ()
clear = callCommand "clear"
