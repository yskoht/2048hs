module App
  ( app
  ) where

import Control.Concurrent
import Control.Monad.State
import System.IO
import Data.List(transpose)

import Types
import Board
import Square
import GetKey
import Console

app :: IO ()
app = do
  board <- initialize
  play board
  terminate

initialize :: IO Board
initialize = do
  hSetBuffering stdin NoBuffering
  board <- execStateT initBoard emptyBoard
  hideCursor
  showBoard' board
  return board

terminate :: IO ()
terminate = do
  showCursor

play :: Board -> IO()
play board = do
  key <- getKey
  case key of
    QuitKey -> return ()
    UnknownKey -> play board
    _ -> do
      newBoard <- update key board
      if isGameOver newBoard
        then gameOver
        else play newBoard

update :: Key -> Board -> IO Board
update key board = do
  newBoard <- updateMove key board
  threadDelay 1500
  updateAdd board newBoard

updateMove :: Key -> Board -> IO Board
updateMove key board = do
  let newBoard = move key board
  let res = renderEvents $ events key board
  let m = eventLen res
  let es = take m $ transpose res
  if null es then return () else render es
  return newBoard

eventLen :: [[a]] -> Int
eventLen ls =
  let xs = filter (< 100) (map length ls)
  in if null xs then 0 else maximum xs

type Event = ([Int], Int, Square)
events :: Key -> Board -> [Event]
events key board =
  let bs = zip ([0..] :: [Int]) (move' key board)
      es = filter (not . isEmpty . snd . snd) bs
  in map f es
  where
    f (next, (prevs, s)) = (prevs, next, s)

type Pos = (Int, Int)

xy :: Int -> Pos
xy n = (n `mod` 4, n `div` 4)

rxy :: Int -> Pos
rxy n =
  let (x, y) = xy n
  in (x * 10 + 1, y * 5 + 2)

type RenderEvent = (Pos, Int)
renderEvents :: [Event] -> [[RenderEvent]]
renderEvents = concatMap f
  where
    f :: Event -> [[RenderEvent]]
    f ([k], n, Number t)
      | k == n = [replicate 100 (rxy k, t)]
      | otherwise =
          if kx == nx
            then
              let rs = if ky > ny
                  then [ky,ky-1..ny]
                  else [ky,ky+1..ny]
              in [zip (zip (repeat kx) rs) (repeat t)]
            else
              let rs = if kx > nx
                  then [kx,kx-1..nx]
                  else [kx,kx+1..nx]
              in [zip (zip rs (repeat ky)) (repeat t)]
        where
          (kx, ky) = rxy k
          (nx, ny) = rxy n
    f ([k1, k2], n, Number t) =
      let a = head (f ([k1], n, Number (t `div` 2))) ++ [(rxy n, t)]
          b = head (f ([k2], n, Number (t `div` 2))) ++ [(rxy n, t)]
      in [a, b]
    f _ = error ""

updateAdd :: Board -> Board -> IO Board
updateAdd oldBoard newBoard = do
  newBoard' <-
    if newBoard == oldBoard
      then
        return newBoard
      else do
        number <- createNumber
        addNumber number newBoard
  showBoard' newBoard'
  return newBoard'

gameOver :: IO ()
gameOver = do
  putStrLn "Game over"

showBoard' :: Board -> IO ()
showBoard' board = do
  clear
  showBoard board

render :: [[RenderEvent]] -> IO()
render ees = do
  forM_ ees $ \es -> do
    showBoard' emptyBoard
    forM_ es $ \(pos, n) -> do
      moveCursor pos
      write n
    moveCursor (0, 0)
    -- threadDelay 5000

moveCursor :: Pos -> IO()
moveCursor (x, y) = do
  putStr $ "\ESC[" ++ show (y+1) ++ ";" ++ show (x+1) ++ "H"
  hFlush stdout

write :: Int -> IO()
write n = do
  putStr $ label (Number n)
  hFlush stdout
