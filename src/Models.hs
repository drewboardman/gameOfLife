module Models where

type Point = (Integer, Integer)

data Cell = Alive | Dead deriving Eq

type Grid = Point -> Cell
