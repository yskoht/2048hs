module App
  ( app
  ) where

import Control.Monad
import System.Random

import Types
import SplitBy
import Readable

initBoard :: Board
initBoard = replicate 16 Empty

showBoard :: Board -> IO ()
showBoard board = do
  let _lines = splitBy4 $ readable board
  forM_ _lines $ \line -> do
    putStrLn $ show line

sample :: [a] -> IO a
sample xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i

createNumber :: IO Square
createNumber =
  sample $ [Number 2, Number 4]

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

sampleEmptyIndex :: Board -> IO Int
sampleEmptyIndex board = do
  let pairs = zip [1..] board
  let empties = filter (\x -> isEmpty (snd x)) pairs
  (i, _) <- sample empties
  return i

replace :: Board -> Int -> Square -> Board
replace board i square =
  let (xs, ys) = splitAt (i-1) board
  in xs ++ [square] ++ tail ys

addNumber :: Board -> Square -> IO Board
addNumber board square = do
  i <- sampleEmptyIndex board
  return $ replace board i square

app :: IO ()
app = do
  b <- addNumber initBoard (Number 2)
  c <- addNumber b (Number 2)
  showBoard c
