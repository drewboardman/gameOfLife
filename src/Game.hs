{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import qualified Models                        as M
import           Data.MemoTrie
import           Graphics.Gloss

dIMENSION :: Float
dIMENSION = 20

gameOfLife :: M.Grid -> Integer -> M.Grid
gameOfLife initial = memo2 go where
  go 0 p = initial p
  go n p = nextStep previousCellStatus previousAdjacentStatus   where
    previousCellStatus :: M.Cell = gameOfLife initial (n - 1) p
    previousAdjacentStatus :: [M.Cell] =
      gameOfLife initial (n - 1) <$> adjacents p

-- how to write this with pattern match
nextStep :: M.Cell -> [M.Cell] -> M.Cell
nextStep M.Alive surrounding = case count M.Alive surrounding of
  alive | alive < 2 -> M.Dead
  alive | alive > 3 -> M.Dead
  _                 -> M.Alive
nextStep M.Dead surrounding = case count M.Alive surrounding of
  alive | alive == 3 -> M.Alive
  _                  -> M.Dead

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

adjacents :: M.Point -> [M.Point]
adjacents (x, y) =
  [ (x + m, y + n) | m <- [-1, 0, 1], n <- [-1, 0, 1], (m, n) /= (0, 0) ]

gameOfLifePicture :: M.Grid -> Picture
gameOfLifePicture g = pictures frames where
  frames =
    [ translate (fromInteger x * dIMENSION)
                (fromInteger y * dIMENSION)
                (color c (rectangleSolid dIMENSION dIMENSION))
    | x :: Integer <- [-50 .. 50]
    , y :: Integer <- [-50 .. 50]
    , let c = chooseColor (g (x, y))
    ]

chooseColor :: M.Cell -> Color
chooseColor M.Alive = red
chooseColor M.Dead  = blue

gameOfLifeAnimation :: (Integer -> M.Grid) -> Float -> Picture
gameOfLifeAnimation f t =
  let time = floor (2 * t)
  in  pictures [gameOfLifePicture (f time), color green (text (show time))]
