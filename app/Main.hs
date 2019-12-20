module Main where

main :: IO ()
main = do
  initial <- loadInitialGrid
  let f = gameOfLife initial
  animate FullScreen white (gameOfLifeAnimation f)
