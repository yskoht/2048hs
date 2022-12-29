{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Board where

import Control.Monad
import Control.Monad.State
import Data.List(transpose)

import Types
import SplitBy
import Readable
import Square
import Sample

empties :: Int -> [Square]
empties n = replicate n Empty

emptyBoard :: Board
emptyBoard = empties 16

showBoard :: Board -> IO ()
showBoard board = do
  let _lines = splitBy4 $ readable board
  forM_ _lines $ \line -> do
    print line

sampleEmptyIndex :: Board -> IO Int
sampleEmptyIndex board = do
  let pairs = zip [1..] board
  let es = filter (isEmpty . snd) pairs
  (i, _) <- sample es
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

move :: Key -> Board -> Board
move LeftKey board =
  let bs = splitBy4 board
  in concatMap (squashLine . moveLine) bs
move RightKey board =
  let bs = splitBy4 board
  in concatMap (reverse . squashLine . moveLine . reverse) bs
move UpKey board =
  let bs = transpose $ splitBy4 board
  in concat $ transpose $ map (squashLine . moveLine) bs
move DownKey board =
  let bs = transpose $ splitBy4 board
  in concat $ transpose $ map (reverse . squashLine . moveLine . reverse) bs
move _ _ = error ""

moveLine :: [Square] -> [Square]
moveLine line =
  numbers ++ empties emptyNum
  where
    emptyNum = length $ filter isEmpty line
    numbers = filter (not . isEmpty) line

squashLine :: [Square] -> [Square]
squashLine xs =
  let (a, b) = foldl squash ([], []) xs
      k = a ++ b
      len = length k
  in k ++ empties (4 - len)
  where
    squash :: ([Square], [Square]) -> Square -> ([Square], [Square])
    squash (line, []) s = (line, [s])
    squash (line, [s]) Empty = (line ++ [s], [Empty])
    squash (line, [Empty]) (Number n) = (line ++ [Empty], [Number n])
    squash (line, [Number t]) (Number n) =
      if t == n
        then (line ++ [Number (t + n)], [])
        else (line ++ [Number t], [Number n])
    squash _ _ = error ""
