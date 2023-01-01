module App
  ( app
  ) where

import Control.Concurrent
import Control.Monad.State
import System.IO

import Types
import Board
import Square
import GetKey
import Console
import Render

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
  showBoard board
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
      if isGameOver newBoard
        then gameOver
        else play newBoard

update :: Key -> Board -> IO Board
update key board = do
  newBoard <- updateMove key board
  showBoard newBoard
  threadDelay (100 * 1000)
  updateAdd board newBoard

updateMove :: Key -> Board -> IO Board
updateMove key board = do
  let as = move key board
  render as
  return $ convertSquare'WithIndexToBoard as

updateAdd :: Board -> Board -> IO Board
updateAdd oldBoard newBoard = do
  if newBoard == oldBoard
    then
      return newBoard
    else do
      number <- createNumber
      newBoard' <- addNumber number newBoard
      showBoard newBoard'
      return newBoard'

gameOver :: IO ()
gameOver = do
  putStrLn "Game over"
