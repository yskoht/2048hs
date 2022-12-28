module SplitBy where

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs =
  let (ys, zs) = splitAt n xs
  in [ys] ++ splitBy n zs

splitBy4 :: [a] -> [[a]]
splitBy4 = splitBy 4
