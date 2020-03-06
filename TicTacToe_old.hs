module TicTacToe where

import qualified Data.List as L

data Cell = X | O | Empty
  deriving (Show, Eq)

type GameState = [Cell]


enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

insertAt :: Integer -> a -> [a] -> [a]
insert i _ _ | i < 0 = error "Cannot insert into list with negative index."
insertAt i e es = L.genericTake i es ++ e : L.genericDrop (i + 1) es

swap :: Cell -> Cell
swap X = O
swap O = X
swap _ = Empty

nextGameStates :: GameState -> Cell -> [GameState]
nextGameStates gs cell = map (\(i, c) -> insertAt i cell gs) openCellGameState
  where
    openCellGameState = filter ((== Empty) . snd) (enumerate gs)

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

minimax :: Cell -> GameState -> Integer
minimax gs cell
  | isGameOver gs = score gs cell
  | otherwise = bestScore
  where
    gameStates = nextGameStates gs cell
    bestScore = maximum $ map ((*(-1)) . minimax $ swap cell) gameStates

findBestMove :: GameState -> Cell -> GameState
findBestMove gs cell =
  L.maximumBy (\(s1, _) (s2, _)-> compare s1 s2) scoreStates
  where
    gameStates = nextGameStates gs cell
    scoreStates = zip (map (minimax cell) gameStates) gameStates

game = [X, O, O, X, X, X, Empty, Empty, X]


main :: IO ()
main = do
  putStr . show $ nextGameStates game O
