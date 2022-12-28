module App
  ( app
  ) where

import Control.Monad

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

app :: IO ()
app = do
  showBoard initBoard
  showBoard $ map (\x -> Number x) [1..16]
