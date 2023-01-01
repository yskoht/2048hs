module Utils
  ( splitBy4,
    sample,
  ) where

import System.Random

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs =
  let (ys, zs) = splitAt n xs
  in ys : splitBy n zs

splitBy4 :: [a] -> [[a]]
splitBy4 = splitBy 4

sample :: [a] -> IO a
sample xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i
