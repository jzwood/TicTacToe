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

insertAt :: [a] -> a -> Integer -> [a]
insert es _ i | i < 0 = es
insertAt es e i = genericTake i es ++ e : genericDrop (i + 1) es

instance MiniMaxable Board where
  --score :: Turn -> Board -> Value
  score isPlayerOneX (c0:c1:tail)
    | winner && (isPlayerOneX == isWinnerX) = 1
    | winner = -1
    | otherwise = score isPlayerOneX (c1:tail)
    where
      winner = c0 == c1 && c0 /= Nothing
      isWinnerX = c0 == Just X
  score _ _ = 0

  --terminal :: Board -> Bool
  terminal board = score True board /= 0 || (not $ elem Nothing board)

  --allMoves :: Turn -> Board -> [Board]
  allMoves isPlayerOneX board = map (insertAt board $ Just (toPlayer isPlayerOneX)) openMoves
    where
      openMoves = map fst $ filter ((== Nothing) . snd) (enumerate board)

board3 = (replicate 9 Nothing) :: Board
board4 = [Nothing,Just X,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing] :: Board
board5 = [Just O,Just X,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing] :: Board

main :: IO ()
main = do
  putStr . show $ board3
  putStr . show $ negamaxalpha 2 True board3 2
  putStr . show $ negamaxalpha 2 False board4 2
  putStr . show $ negamaxalpha 2 True board5 2
