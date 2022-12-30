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
  board <- initialize
  play board
  terminate

initialize :: IO Board
initialize = do
  hSetBuffering stdin NoBuffering
  board <- execStateT initBoard emptyBoard
  hideCursor
  showBoard' board
  return board

terminate :: IO ()
terminate = do
  showCursor

play :: Board -> IO()
play board = do
  key <- getKey
  case key of
    QuitKey -> return ()
    UnknownKey -> play board
    _ -> do
      newBoard <- update key board
      showBoard' newBoard
      if isGameOver newBoard
        then gameOver
        else play newBoard

update :: Key -> Board -> IO Board
update key board = do
  let newBoard = move key board
  showBoard' newBoard
  sleep 1500
  if newBoard == board
    then
      return newBoard
    else do
      number <- createNumber
      addNumber number newBoard

gameOver :: IO ()
gameOver = do
  putStrLn "Game over"

clear :: IO ()
clear = callCommand "clear"

hideCursor :: IO ()
hideCursor = putStrLn "\ESC[?25l"

showCursor :: IO ()
showCursor = putStrLn "\ESC[?25h"

sleep :: Int -> IO ()
sleep = threadDelay

showBoard' :: Board -> IO ()
showBoard' board = do
  clear
  showBoard board
