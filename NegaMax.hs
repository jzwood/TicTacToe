module NegaMax where

import Data.List (genericTake, genericDrop, maximumBy)

data Player = X | O
  deriving (Show, Read, Eq)
type Cell = Maybe Player
type Board = [Cell]

alternate :: Player -> Player
alternate X = O
alternate O = X

score :: Board -> Player -> Integer
score (c0:c1:c2:[]) p1
  | winner && c1 == Just p1 = 1
  | winner && c1 == Just p2 = -1
  | otherwise = 0
  where
    p2 = alternate p1
    winner =  c0 == c1 || c1 == c2

terminal :: Board -> Bool
terminal board = score board X /= 0 || (not $ elem Nothing board)

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

insertAt :: [a] -> a -> Integer -> [a]
insert es _ i | i < 0 = es
insertAt es e i = genericTake i es ++ e : genericDrop (i + 1) es

-- VERIFIED
allMoves :: Board -> Player -> [Board]
allMoves board player = map (insertAt board $ Just player) openMoves
  where
    openMoves = map fst $ filter ((== Nothing) . snd) (enumerate board)

-- VERIFIED
maximumBySnd :: (Ord b, Foldable f) => f (a, b) -> (a, b)
maximumBySnd = maximumBy (\(_, s1) (_, s2) -> compare s1 s2)


negamax :: Player -> Board -> (Board, Integer)
negamax player board
    | terminal board = ([], score board player)
    | otherwise = maximumBySnd $ zip moves values
  where
    moves = allMoves board player
    values = map (negate . snd . (negamax $ alternate player)) moves

startBoard = [Nothing, Nothing, Nothing]
sndBoard = [Nothing, Just X, Nothing]
trdBoard = [Nothing, Just X, Just O]

main :: IO ()
main = do
  putStr . show $ negamax X startBoard
  putStr . show $ negamax O sndBoard
  putStr . show $ negamax X trdBoard
