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
move key board
  | key == LeftKey  = concatMap moveAndSquash bs
  | key == RightKey = concatMap moveAndSquashRev bs
  | key == UpKey    = concat $ transpose $ map moveAndSquash $ transpose bs
  | key == DownKey  = concat $ transpose $ map moveAndSquashRev $ transpose bs
  | otherwise = error ""
  where
    bs = splitBy4 board
    moveAndSquash = squashLine . moveLine
    moveAndSquashRev = reverse . moveAndSquash . reverse

type LabeledSquare = ([Int], Square)
type LabeledBoard = [LabeledSquare]

move' :: Key -> Board -> LabeledBoard
move' key board
  | key == LeftKey  = concatMap moveAndSquash' bs
  | key == RightKey = concatMap moveAndSquashRev' bs
  | key == UpKey    = concat $ transpose $ map moveAndSquash' $ transpose bs
  | key == DownKey  = concat $ transpose $ map moveAndSquashRev' $ transpose bs
  | otherwise = error ""
  where
    bs = splitBy4 $ labeled [0..] board
    moveAndSquash' = squashLine' . moveLine'
    moveAndSquashRev' = reverse . moveAndSquash' . reverse

labeled :: [Int] -> [Square] -> [LabeledSquare]
labeled = zipWith (\i s -> ([i], s))

moveLine' :: [LabeledSquare] -> [LabeledSquare]
moveLine' line =
  numbers ++ labeled (repeat (-1)) (empties emptyNum)
  where
    emptyNum = length $ filter (isEmpty . snd) line
    numbers = filter (not . isEmpty . snd) line

squashLine' :: [LabeledSquare] -> [LabeledSquare]
squashLine' xs =
  let (as, bs, cs) = foldl squash ([], [], []) xs
  in as ++ bs ++ cs
  where
    squash :: ([LabeledSquare], [LabeledSquare], [LabeledSquare]) -> LabeledSquare -> ([LabeledSquare], [LabeledSquare], [LabeledSquare])
    squash (line, [], es) s = (line, [s], es)
    squash (line, [s], es) t@(_, Empty) = (line ++ [s], [t], es)
    squash (line, [u@(_, Empty)], es) t = (line ++ [u], [t], es)
    squash (line, [u@(i, Number t)], es) v@(j, Number n) =
      if t == n
        then (line ++ [(i ++ j, Number (t + n))], [], es ++ [([-1], Empty)])
        else (line ++ [u], [v], es)
    squash _ _ = error ""

moveLine :: [Square] -> [Square]
moveLine line =
  numbers ++ empties emptyNum
  where
    emptyNum = length $ filter isEmpty line
    numbers = filter (not . isEmpty) line

squashLine :: [Square] -> [Square]
squashLine xs =
  let (as, bs, cs) = foldl squash ([], [], []) xs
  in as ++ bs ++ cs
  where
    squash :: ([Square], [Square], [Square]) -> Square -> ([Square], [Square], [Square])
    squash (line, [], es) s = (line, [s], es)
    squash (line, [s], es) Empty = (line ++ [s], [Empty], es)
    squash (line, [Empty], es) (Number n) = (line ++ [Empty], [Number n], es)
    squash (line, [Number t], es) (Number n) =
      if t == n
        then (line ++ [Number (t + n)], [], es ++ [Empty])
        else (line ++ [Number t], [Number n], es)
    squash _ _ = error ""

emptyNotExists :: Board -> Bool
emptyNotExists board = not $ any isEmpty board

adjacentSameNumberNotExists :: [[Square]] -> Bool
adjacentSameNumberNotExists = foldl f True
  where
    f :: Bool -> [Square] -> Bool
    f acc (x:xs) = acc && snd (foldl g (x, True) xs)
    f _ _ = error ""
    g :: (Square, Bool) -> Square -> (Square, Bool)
    g (_, False) _ = (Empty, False)
    g (Empty, _) _ = (Empty, False)
    g _ Empty = (Empty, False)
    g (Number n, _) (Number m) = if n == m then (Number m, False) else (Number m, True)

isGameOver :: Board -> Bool
isGameOver board = emptyNotExists board
  && adjacentSameNumberNotExists _lines
  && adjacentSameNumberNotExists linesT
  where
    _lines = splitBy4 board
    linesT = transpose _lines

