{-# LANGUAGE FlexibleInstances #-}

module TwoInARow where

import Data.List (genericTake, genericDrop)
import MiniMax

data Player = X | O
  deriving (Show, Read, Eq)
type Cell = Maybe Player
type Board = [Cell]
type Value = Integer

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

toPlayer :: Bool -> Player
toPlayer isPlayerOneX = if isPlayerOneX then X else O

alternate :: Player -> Player
alternate X = O
alternate O = X

insertAt :: [a] -> a -> Integer -> [a]
insert es _ i | i < 0 = es
insertAt es e i = genericTake i es ++ e : genericDrop (i + 1) es

instance MiniMaxable Board where
  --score :: Turn -> Board -> Value
  score isPlayerOneX (c0:c1:c2:[])
    | noWinner = 0
    | xWon == isPlayerOneX = 1
    | otherwise = -1
    where
      noWinner =  c0 /= c1 && c1 /= c2 || c1 == Nothing
      xWon = c1 == Just X

  --terminal :: Board -> Bool
  terminal board = score True board /= 0 || (not $ elem Nothing board)

  --allMoves :: Turn -> Board -> [Board]
  allMoves isPlayerOneX board = map (insertAt board $ Just (toPlayer isPlayerOneX)) openMoves
    where
      openMoves = map fst $ filter ((== Nothing) . snd) (enumerate board)
      player = toPlayer isPlayerOneX

startBoard = [Nothing, Nothing, Nothing] :: Board
board2 = [Nothing, Just X, Nothing] :: Board

main :: IO ()
main = do
  putStr . show $ negamax True startBoard
  putStr . show $ negamax False board2
