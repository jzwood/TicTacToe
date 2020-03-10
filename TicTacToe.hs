{-# LANGUAGE FlexibleInstances #-}

module TicTacToe where

import Data.List (genericTake, genericDrop, transpose)
import MiniMax

data Player = X | O
  deriving (Show, Read, Eq)
type Cell = Maybe Player
type Board = [[Cell]]
type Value = Integer

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

toPlayer :: Bool -> Player
toPlayer isPlayerOneX = if isPlayerOneX then X else O

chunk3 :: [a] -> [[a]]
chunk3 (a:b:c:xs) = [a, b, c] : chunk3 xs
chunk3 _ = []

insertAt :: [a] -> a -> Integer -> [a]
insert es _ i | i < 0 = es
insertAt es e i = genericTake i es ++ e : genericDrop (i + 1) es

winsOrtho :: Board -> Player -> Bool
winsOrtho board player =
  let
    threeInARow :: [Maybe Player] -> Bool -> Bool
    threeInARow (a:b:c:[]) w = a == b && b == c && b == Just player && w
    threeInARow _ bool = bool
    wins = foldr threeInARow True
  in
    wins board || (wins $ transpose board)

winsDiagonal :: Board -> Player -> Bool
winsDiagonal board player =
  let
    diagonal xxs = map (\(i, xs) -> xs !! i) $ zip [0..] xxs
    allPlayer player = all (== Just player)
  in
    (allPlayer player $ diagonal board) || (allPlayer player $ diagonal $ reverse board)

wins :: Board -> Player -> Bool
wins board player = winsOrtho board player || winsDiagonal board player

instance MiniMaxable Board where
  --score :: Turn -> Board -> Value
  score isPlayerOneX board
    | noWinner = 0
    | xWon == isPlayerOneX = 1
    | otherwise = -1
    where
      xWon = wins board X
      oWon = wins board O
      noWinner = not xWon && not oWon

  --terminal :: Board -> Bool
  terminal board = score True board /= 0 || (not $ elem Nothing $ concat board)

  --allMoves :: Turn -> Board -> [Board]
  allMoves isPlayerOneX board = moves
    where
      flatBoard = concat board
      openIndices = map fst $ filter ((== Nothing) . snd) (enumerate $ flatBoard)
      moves = chunk3 $ map (insertAt flatBoard (Just $ toPlayer isPlayerOneX)) openIndices

startBoard = [[Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing]] :: Board


main :: IO ()
main = do
  putStr . show $ negamax True startBoard
