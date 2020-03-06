module TicTacToe where

import qualified Data.List as L

data Cell = X | O | Empty
  deriving (Show, Eq)

type GameState = [Cell]

wins :: GameState -> Cell -> Bool
wins (c00:c01:c02:c10:c11:c12:c20:c21:c22:[]) cell =
  (c00 == c01 && c00 == c02 && c00 == cell) ||
  (c10 == c11 && c10 == c12 && c10 == cell) ||
  (c20 == c21 && c20 == c22 && c20 == cell) ||
  (c00 == c10 && c00 == c20 && c00 == cell) ||
  (c01 == c11 && c01 == c20 && c01 == cell) ||
  (c02 == c12 && c02 == c20 && c02 == cell) ||
  (c00 == c11 && c11 == c22 && c00 == cell) ||
  (c02 == c11 && c11 == c02 && c02 == cell)

score :: GameState -> Cell -> Integer
score gs cell
  | wins gs cell = 1
  | wins gs (swap cell) = -1
  | otherwise = 0

isGameOver :: GameState -> Bool
isGameOver gs = score gs O /= 0 || find (== Empty) == Nothing
