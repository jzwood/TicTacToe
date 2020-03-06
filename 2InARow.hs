module TwoInARow where

import TicTacToe
import qualified Data.List as L

type Board = [Cell]

wins :: GameState -> Cell -> Bool
wins (c0:c1:c2:[]) cell =
  c0 == c1 || c1 == c2 && c1 == cell

score :: GameState -> Cell -> Integer
score gs cell
  | wins gs cell = 1
  | wins gs (swap cell) = -1
  | otherwise = 0

isGameOver :: GameState -> Bool
isGameOver gs = score gs O /= 0 || find (== Empty) == Nothing
