module Console
  ( clear,
    hideCursor,
    showCursor,
    moveCursor,
    write,
    disableBuffering,
  ) where

import System.Process
import System.IO

clear :: IO ()
clear = callCommand "clear"

hideCursor :: IO ()
hideCursor = putStrLn "\ESC[?25l"

showCursor :: IO ()
showCursor = putStrLn "\ESC[?25h"

moveCursor :: Int -> Int -> IO()
moveCursor x y = do
  putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"
  hFlush stdout

write :: String -> IO ()
write str = do
  putStr str
  hFlush stdout

disableBuffering :: IO ()
disableBuffering =
  hSetBuffering stdin NoBuffering
