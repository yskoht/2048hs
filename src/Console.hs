module Console
  ( clear,
    hideCursor,
    showCursor,
    write,
  ) where

import System.Process
import System.IO

clear :: IO ()
clear = callCommand "clear"

hideCursor :: IO ()
hideCursor = putStrLn "\ESC[?25l"

showCursor :: IO ()
showCursor = putStrLn "\ESC[?25h"

write :: String -> IO ()
write str = do
  putStr str
  hFlush stdout
