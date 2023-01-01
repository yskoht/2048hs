module Render
  ( render,
  ) where

import Control.Monad
import Control.Concurrent
import Data.List(transpose)

import Board
import Types
import Console
import Square

data Pos = Pos { x :: Int, y :: Int } deriving (Show)
type RenderingData = (Pos, Int)

_pos :: Int -> Int -> Pos
_pos n index = Pos { x = index `mod` n, y = index `div` n }

pos :: Int -> Pos
pos = _pos 4

renderingPos :: Int -> Pos
renderingPos index =
  let (Pos x y) = pos index
  in Pos { x = x * 10 + 1, y = y * 5 + 2 }

render :: [Square'WithIndex] -> IO ()
render as = _render $ renderingData as
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

_render :: [[RenderingData]] -> IO()
_render dss = do
  forM_ dss $ \ds -> do
    showBoard emptyBoard
    forM_ ds $ \(Pos x y, n) -> do
      moveCursor (x+1) (y+1)
      write $ label $ Number n
    moveCursor 0 0
    threadDelay 1000
