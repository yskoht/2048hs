{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Board
  ( emptyBoard,
    showBoard,
    addNumber,
    initBoard,
    convertSquare'WithIndexToBoard,
    move,
    isGameOver,
    Square'(..),
    Square'WithIndex,
  ) where

import Control.Monad
import Control.Monad.State
import Data.List(transpose)

import Types
import Square
import Utils
import Console

data NumberWithIndex = NumberWithIndex { _index :: Int, number :: Int }
data Square' = Square' { value :: Square, fromIndexes :: [Int] } deriving (Show)
type Square'WithIndex = (Int, Square')

emptyBoard :: Board
emptyBoard = replicate 16 Empty

_showBoard :: Board -> IO ()
_showBoard board = do
  let _lines = splitBy4 $ map Square.fmt board
  forM_ _lines $ \line -> do
    forM_ (transpose line) $ \xs -> do
      putStrLn $ concat xs

showBoard :: Board -> IO ()
showBoard board = do
  clear
  _showBoard board

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

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

_numberWithIndex :: ([[(Int, Square)]] -> [[(Int, Square)]]) -> Board -> [[NumberWithIndex]]
_numberWithIndex t = convert . filter' . t . splitBy4 . withIndex
  where
    -- only Number
    filter' :: [[(Int, Square)]] -> [[(Int, Square)]]
    filter' = map f
      where
        f :: [(Int, Square)] -> [(Int, Square)]
        f = filter (not . isEmpty . snd)

    convert :: [[(Int, Square)]] -> [[NumberWithIndex]]
    convert = map f
      where
        f :: [(Int, Square)] -> [NumberWithIndex]
        f = map g
        g (i, Number n) = NumberWithIndex i n
        g _ = error ""

numberWithIndex :: Board -> [[NumberWithIndex]]
numberWithIndex = _numberWithIndex id

numberWithIndexT :: Board -> [[NumberWithIndex]]
numberWithIndexT = _numberWithIndex transpose

convertSquare'WithIndexToBoard :: [Square'WithIndex] -> Board
convertSquare'WithIndexToBoard  = map f
  where
    f :: Square'WithIndex -> Square
    f (_, Square' s _) = s

move :: Key -> Board -> [Square'WithIndex]
move key board = withIndex $ move' key board
  where
    move' key' board'
      | key' == LeftKey  = concatMap squash ns
      | key' == RightKey = concatMap squashRev ns
      | key' == UpKey    = concat $ transpose $ map squash nsT
      | key' == DownKey  = concat $ transpose $ map squashRev nsT
      | otherwise = error ""
      where
        ns = numberWithIndex board'
        nsT = numberWithIndexT board'
        squashRev = reverse . squash . reverse

data T = T { fixes :: [Square'], candidate :: Maybe NumberWithIndex }
squash :: [NumberWithIndex] -> [Square']
squash xs = fillEmpty4 $ wrapUp $ foldl squash' initT xs
  where
    initT = T { fixes = [], candidate = Nothing }

    convert :: NumberWithIndex -> Square'
    convert (NumberWithIndex i n) = Square' { value = Number n, fromIndexes = [i] }

    add :: NumberWithIndex -> NumberWithIndex -> Square'
    add (NumberWithIndex ai an) (NumberWithIndex bi bn) =
      Square' { value = Number (an + bn), fromIndexes = [ai, bi] }

    squash' :: T -> NumberWithIndex -> T
    squash' (T f Nothing) n = T f (Just n)
    squash' (T f (Just c)) n
      | number c == number n = T (f ++ [add c n]) Nothing
      | otherwise = T (f ++ [convert c]) (Just n)

    wrapUp :: T -> [Square']
    wrapUp (T f Nothing) = f
    wrapUp (T f (Just c)) = f ++ [convert c]

    fillEmpty4 :: [Square'] -> [Square']
    fillEmpty4 = fillEmpty 4
      where
        fillEmpty :: Int -> [Square'] -> [Square']
        fillEmpty n zs = zs ++ replicate (n - length zs) empty
          where
            empty = Square' { value = Empty, fromIndexes = [] }

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
