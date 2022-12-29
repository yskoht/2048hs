module Sample where

import System.Random

sample :: [a] -> IO a
sample xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i
