module Console
  ( clear,
    disableEcho,
    enableEcho,
    hideCursor,
    showCursor,
    moveCursor,
    write,
    disableBuffering,
    getKey,
  ) where

import System.Process
import System.IO
import Data.Char

import Types

clear :: IO ()
clear = callCommand "clear"

disableEcho :: IO ()
disableEcho = callCommand "stty -echo"

enableEcho :: IO ()
enableEcho = callCommand "stty echo"

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

getKey :: IO Key
getKey = do
  c <- getChar
  case ord c of
    27 -> getKey
    91 -> getKey
    65 -> return UpKey -- 27 91 65 -> Up
    66 -> return DownKey -- 27 91 66 -> Down
    67 -> return RightKey -- 27 91 67 -> Right
    68 -> return LeftKey -- 27 91 68 -> Left
    104 -> return LeftKey -- h
    106 -> return DownKey -- j
    107 -> return UpKey -- k
    108 -> return RightKey -- l
    113 -> return QuitKey -- q
    _ -> return UnknownKey
