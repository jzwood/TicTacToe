module PlayerGround where

import Data.List (genericTake, genericDrop, maximumBy)

data Player = X | O
  deriving (Show, Read, Eq)
type Cell = Maybe Player
type Board = [Cell]

alternate :: Player -> Player
alternate X = O
alternate O = X

t = [Nothing, Nothing]


solveLogic :: Int -> Int -> Int
solveLogic a b =
    let
        x = 1
    in
        | a >= x     = 1
        | a == b     = 333
        | otherwise  = 5
--score :: Board -> Player -> Integer
--score (c0:c1:c2:[]) p1
  -- winner && c1 == Just p1 = 1
  -- winner && c1 == Just p2 = -1
  -- otherwise = 0
  --where
    --p2 = alternate p1
    --winner =  c0 == c1 || c1 == c2
