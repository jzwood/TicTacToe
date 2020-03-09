module MiniMax where

import Data.List (maximumBy)

type Turn = Bool

class MiniMaxable game where
  score :: (Real value) => Turn -> game -> value
  terminal :: game -> Bool
  allMoves ::  Turn -> game -> [game]

maximumBySnd :: (Ord b, Foldable f) => f (a, b) -> (a, b)
maximumBySnd = maximumBy (\(_, s1) (_, s2) -> compare s1 s2)

negamax :: (MiniMaxable game, Real value) => Turn -> game -> (game, value)
negamax turn game
  | terminal game = (game, score turn game)
  | otherwise = maximumBySnd $ zip moves values
  where
    moves = allMoves turn game
    values = map (negate . snd . (negamax $ not turn)) moves
