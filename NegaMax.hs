module NegaMax where

import qualified Data.List as L

data Player = X | O
data Cell = Player | Empty
type Board = [Player]


invert :: Player -> Player
invert X = O
invert O = X

score :: Board -> Player -> Integer
score (c0:c1:c2:[]) p1
  | winner && c1 == p1 = 1
  | winner && c1 == p2 = -1
  | otherwise = 0
  where
    p2 = invert p1
    winner =  c0 == c1 || c1 == c2 && c1 == p1

terminal :: Board -> Bool
terminal board = score board O /= 0 || find (== Empty) == Nothing

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

insertAt :: [a] -> a -> Integer -> [a]
insert i _ _ | i < 0 = error "Cannot insert into list with negative index."
insertAt i e es = L.genericTake i es ++ e : L.genericDrop (i + 1) es

allMoves :: Board -> Player -> [Board]
allMoves board player = map (insertAt board player) openMoves
  where
    openMoves = map fst $ filter ((== Empty) . snd) (enumerate board)

maximumBySnd :: (Ord b) => f (a, b) -> (a, b)
maximumBySnd = L.maximumBy (\(_, s1) (_, s2) -> compare s1 s2)

negamax :: Board -> Player -> (Board, Integer)
negamax board player
  | terminal board = score board player
  | otherwise = (\(a, i) -> (a, -i)) bestMove  -- negates score
  where
    moves = allMoves board player
    bestMove = maximumBySnd $ map (negamax $ invert player) moves


startBoard = [Empty, Empty, Empty]


main :: IO ()
main = do
  putStr . show $ negamax startBoard O
