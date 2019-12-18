{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import           Models

gameOfLife :: Grid -> Integer -> Grid
gameOfLife initial 0 p = initial p
gameOfLife initial n p = nextStep previousCellStatus previousAdjacentStatus where
  previousCellStatus :: Cell       = gameOfLife initial (n - 1) p
  previousAdjacentStatus :: [Cell] = gameOfLife initial (n - 1) <$> adjacents p

iterate :: Grid -> Integer -> Grid
iterate initial 0 = initial
iterate initial n p =  

-- how to write this with pattern match
nextStep :: Cell -> [Cell] -> Cell
nextStep Alive surrounding | count Alive surrounding < 2 = Dead
                           | count Alive surrounding > 3 = Dead
                           | otherwise                   = Alive
nextStep Dead surrounding | count Alive surrounding == 3 = Alive
                          | otherwise                    = Dead

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

adjacents :: Point -> [Point]
adjacents (x, y) =
  [ (x + m, y + n) | m <- [-1, 0, 1], n <- [-1, 0, 1], (m, n) /= (0, 0) ]
