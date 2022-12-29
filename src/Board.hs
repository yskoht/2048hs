{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Board where

import Control.Monad
import Control.Monad.State

import Types
import SplitBy
import Readable
import Square
import Sample

emptyBoard :: Board
emptyBoard = replicate 16 Empty

showBoard :: Board -> IO ()
showBoard board = do
  let _lines = splitBy4 $ readable board
  forM_ _lines $ \line -> do
    print line

sampleEmptyIndex :: Board -> IO Int
sampleEmptyIndex board = do
  let pairs = zip [1..] board
  let empties = filter (isEmpty . snd) pairs
  (i, _) <- sample empties
  return i

replace :: Board -> Int -> Square -> Board
replace board i square =
  let (xs, ys) = splitAt (i-1) board
  in xs ++ [square] ++ tail ys

addNumber :: Square -> Board -> IO Board
addNumber square board = do
  i <- sampleEmptyIndex board
  return $ replace board i square

addNumberS :: Square -> StateIO Board Board
addNumberS square = do
  board <- get
  newBoard <- lift $ addNumber square board
  put newBoard
  return newBoard

initBoard :: StateIO Board Board
initBoard = do
  addNumberS (Number 2)
  addNumberS (Number 2)
