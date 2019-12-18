{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import           Models

gameOfLife :: Grid -> Integer -> Grid
gameOfLife initial 0 p = initial p
gameOfLife initial n p = nextStep current adjacentCells where
  current :: Cell         = gameOfLife initial (n - 1) p
  adjacentCells :: [Cell] = gameOfLife initial (n - 1) <$> adjacents p

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
