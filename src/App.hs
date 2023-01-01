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
  showBoard' newBoard
  threadDelay (100 * 1000)
  updateAdd board newBoard

updateMove :: Key -> Board -> IO Board
updateMove key board = do
  let as = move key board
  render' as
  return $ convertSquare'WithIndexToBoard as

data Pos = Pos { x :: Int, y :: Int } deriving (Show)
_pos :: Int -> Int -> Pos
_pos n index = Pos { x = index `mod` n, y = index `div` n }

pos :: Int -> Pos
pos = _pos 4

renderingPos :: Int -> Pos
renderingPos index =
  let (Pos x y) = pos index
  in Pos { x = x * 10 + 1, y = y * 5 + 2 }

type RenderingData = (Pos, Int)

render' :: [Square'WithIndex] -> IO ()
render' as = render $ renderingData as
  where
    renderingData :: [Square'WithIndex] -> [[RenderingData]]
    renderingData as' = transpose $ fill $ concatMap f as'
        where
          f :: Square'WithIndex -> [[RenderingData]]
          f (to, Square' (Number n) [from])
            | to == from = [[(renderingPos to, n)]]
            | toX == fromX =
              let ys = if fromY < toY
                  then [fromY, fromY+1 .. toY]
                  else [fromY, fromY-1 .. toY]
              in [makeRenderingData (makePosY toX ys) n]
            | otherwise =
              let xs = if fromX < toX
                  then [fromX, fromX+1 .. toX]
                  else [fromX, fromX-1 .. toX]
              in [makeRenderingData (makePosX toY xs) n]
            where
              Pos toX toY = renderingPos to
              Pos fromX fromY = renderingPos from

              makePosX :: Int -> [Int] -> [Pos]
              makePosX y = map (`Pos` y)

              makePosY :: Int -> [Int] -> [Pos]
              makePosY x = map (Pos x)

              makeRenderingData :: [Pos] -> Int -> [RenderingData]
              makeRenderingData ps n' = zip ps (repeat n')
          f (to, Square' (Number n) [from1, from2]) =
            if length ds1 < length ds2
              then [ds1, ds2 ++ g]
              else [ds1 ++ g, ds2]
            where
              orgN = n `div` 2
              ds1 = head $ f (to, Square' (Number orgN) [from1])
              ds2 = head $ f (to, Square' (Number orgN) [from2])
              g = [(renderingPos to, n)]
          f _ = [[]]

          fill :: [[RenderingData]] -> [[RenderingData]]
          fill xss = map g xss'
            where
              xss' = filter (not . null) xss
              m = maximum $ map length xss'
              g xs = xs ++ replicate (m - length xs) (last xs)

updateAdd :: Board -> Board -> IO Board
updateAdd oldBoard newBoard = do
  if newBoard == oldBoard
    then
      return newBoard
    else do
      number <- createNumber
      newBoard' <- addNumber number newBoard
      showBoard' newBoard'
      return newBoard'

gameOver :: IO ()
gameOver = do
  putStrLn "Game over"

showBoard' :: Board -> IO ()
showBoard' board = do
  clear
  showBoard board

render :: [[RenderingData]] -> IO()
render dss = do
  forM_ dss $ \ds -> do
    showBoard' emptyBoard
    forM_ ds $ \(p, n) -> do
      moveCursor p
      write n
    moveCursor (Pos 0 0)
    threadDelay 1000

moveCursor :: Pos -> IO()
moveCursor (Pos x y) = do
  putStr $ "\ESC[" ++ show (y+1) ++ ";" ++ show (x+1) ++ "H"
  hFlush stdout

write :: Int -> IO()
write n = do
  putStr $ label (Number n)
  hFlush stdout
