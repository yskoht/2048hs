{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Board where

import Control.Monad
import Control.Monad.State
import Data.List(transpose)

import Types
import SplitBy
import Square
import Sample

empties :: Int -> [Square]
empties n = replicate n Empty

emptyBoard :: Board
emptyBoard = empties 16

showBoard :: Board -> IO ()
showBoard board = do
  let _lines = splitBy4 $ map Square.fmt board
  forM_ _lines $ \line -> do
    forM_ (transpose line) $ \xs -> do
      putStrLn $ concat xs

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

emptyNotExists :: Board -> Bool
emptyNotExists board = length es == 0
  where
    es = filter isEmpty board

adjacentSameNumberNotExists :: [[Square]] -> Bool
adjacentSameNumberNotExists = foldl f True
  where
    f :: Bool -> [Square] -> Bool
    f acc (x:xs) = acc && (snd $ foldl g (x, True) xs)
    f _ _ = error ""
    g :: (Square, Bool) -> Square -> (Square, Bool)
    g (_, False) _ = (Empty, False)
    g (Empty, _) _ = (Empty, False)
    g _ Empty = (Empty, False)
    g (Number n, _) (Number m) = if n == m then (Number m, False) else (Number m, True)

gameOver :: Board -> Bool
gameOver board =
     (emptyNotExists board)
  && (adjacentSameNumberNotExists _lines)
  && (adjacentSameNumberNotExists linesT)
  where
    _lines = splitBy4 board
    linesT = transpose _lines
