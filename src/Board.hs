{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Board
  ( emptyBoard,
    showBoard,
    addNumber,
    initBoard,
    convAtoBoard,
    move,
    isGameOver,
    A(..),
  ) where

import Control.Monad
import Control.Monad.State
import Data.List(transpose)

import Types
import SplitBy
import Square
import Sample

emptyBoard :: Board
emptyBoard = replicate 16 Empty

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

data NumberWithIndex = NumberWithIndex { _index :: Int, number :: Int }

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

data A = A { value :: Square, fromIndexes :: [Int] } deriving (Show)
data B = B { fixes :: [A], candidate :: Maybe NumberWithIndex }

convAtoBoard :: [(Int, A)] -> Board
convAtoBoard = map f
  where
    f :: (Int, A) -> Square
    f (_, A s _) = s

move :: Key -> Board -> [(Int, A)]
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

squash :: [NumberWithIndex] -> [A]
squash xs =
  let as = foldl squash' B{ fixes = [], candidate = Nothing } xs
  in fillEmpty4 $ append as
  where
    conv :: NumberWithIndex -> A
    conv (NumberWithIndex i n) = A { value = Number n, fromIndexes = [i] }

    add :: NumberWithIndex -> NumberWithIndex -> A
    add (NumberWithIndex ai an) (NumberWithIndex bi bn) = A { value = Number (an + bn), fromIndexes = [ai, bi] }

    squash' :: B -> NumberWithIndex -> B
    squash' (B f Nothing) n = B f (Just n)
    squash' (B f (Just c)) n
      | number c == number n = B (f ++ [add c n]) Nothing
      | otherwise = B (f ++ [conv c]) (Just n)

    append :: B -> [A]
    append (B f Nothing) = f
    append (B f (Just c)) = f ++ [conv c]

    fillEmpty4 :: [A] -> [A]
    fillEmpty4 = fillEmpty 4
      where
        fillEmpty :: Int -> [A] -> [A]
        fillEmpty n zs = zs ++ replicate m (A{ value = Empty, fromIndexes = [] })
          where
            m = n - length zs

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

