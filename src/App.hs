module App
  ( app
  ) where

import Control.Concurrent
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
  hideCursor
  clear
  showBoard board
  play board
  showCursor

play :: Board -> IO()
play board = do
  key <- getKey
  case key of
    QuitKey -> return ()
    UnknownKey -> play board
    _ -> do
      let newBoard = move key board
      clear
      showBoard newBoard
      sleep 1500
      newBoard2 <- if newBoard /= board
                    then do
                      number <- createNumber
                      execStateT (addNumberS number) newBoard
                    else return newBoard
      clear
      showBoard newBoard2
      if gameOver newBoard2
        then putStrLn "Game over"
        else play newBoard2

clear :: IO ()
clear = callCommand "clear"

hideCursor :: IO ()
hideCursor = putStrLn "\ESC[?25l"

showCursor :: IO ()
showCursor = putStrLn "\ESC[?25h"

sleep :: Int -> IO ()
sleep = threadDelay
