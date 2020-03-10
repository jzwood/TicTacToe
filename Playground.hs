module PlayerGround where

import Data.List (genericTake, genericDrop, maximumBy)

data Player = X | O
  deriving (Show, Read, Eq)
type Cell = Maybe Player
type Board = [Cell]

alternate :: Player -> Player
alternate X = O
alternate O = X

t = [Nothing, Nothing]

chunk :: [a] -> [[a]]
chunk (a:b:c:xs) = [a,b,c] : chunk xs
chunk _ = []

--chunk' :: Integer -> [a] -> [[a]]
--chunk' i (x:xs) = h : chunk' t
  --where
    --h = take i xs
    --t = drop i xs
--chunk' _ _ = []

main :: IO ()
main = do
  putStr . show $ chunk [0..22]
