module MiniMax where

import Data.List (maximumBy, minimumBy, takeWhile)

type Turn = Bool

class MiniMaxable game where
  score :: (Real value) => Turn -> game -> value
  terminal :: game -> Bool
  allMoves ::  Turn -> game -> [game]

maximumBySnd :: (Ord b, Foldable f) => f (a, b) -> (a, b)
maximumBySnd = maximumBy (\(_, s1) (_, s2) -> compare s1 s2)

minimumBySnd :: (Ord b, Foldable f) => f (a, b) -> (a, b)
minimumBySnd = minimumBy (\(_, s1) (_, s2) -> compare s1 s2)

negamax :: (MiniMaxable game, Real value) => Turn -> game -> (game, value)
negamax turn game
  | terminal game = (game, score turn game)
  | otherwise = best
  where
    moves = allMoves turn game
    values = map (negate . snd . (negamax $ not turn)) moves
    best = maximumBySnd $ zip moves values

negamax' :: (MiniMaxable game, Real value) => Turn -> game -> (game, value)
negamax' turn game
    | terminal game = (game, score turn game)
    | otherwise = best
  where
    moves = allMoves turn game
    values = map (snd . (negamax' $ not turn)) moves
    opponentBest = minimumBySnd $ zip moves values
    best = (\(b, v) -> (b, -v)) opponentBest

-- running min map: just like map but running minimum value is passed to each f
rmmap :: (Real b) => b -> (a -> b -> (a, b)) -> [a] -> [b]
rmmap m f [] = []
rmmap m f (x:xs) = smallest : rmmap smallest f xs
  where smallest = min m (snd $ f x m)

negamaxalpha :: (MiniMaxable game, Real value) => Turn -> game -> value -> (game, value)
negamaxalpha turn game alpha =
  if terminal game then (game, score turn game) else best
    where
      moves = allMoves turn game
      values = rmmap 2 (negamaxalpha (not turn)) moves
      prunedValues = takeWhile (> alpha) values
      opponentBest = minimumBySnd $ zip moves values
      best = (\(b, v) -> (b, -v)) opponentBest
