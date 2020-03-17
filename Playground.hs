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

-- foldr to BOTH min-early-quit and reduce to min
--test = foldr (\x (a, continue) -> if continue then (x : a, x < 5) else (a, False))
        --([], False) [0..10]

mm :: (Real b ) => b -> (Bool, b) -> (Bool, b)
mm nmin (break, cmin)
  | break = (True, cmin)
  | cmin == 3 = (True, cmin)
  | nmin < cmin = (False, nmin)
  | otherwise = (False, cmin)

min' :: (Real a) => a -> a -> [a] -> a
min' _ smallest [] = smallest
min' alpha smallest (x:xs)
  | smallest < alpha = smallest
  | x < smallest = min' alpha x xs
  | otherwise = min' alpha smallest xs


rmap :: (Ord b) => b -> (a -> b) -> [a] -> [b]
rmap m f [] = []
rmap m f (x:xs) = smallest : rmap smallest f xs
  where smallest = min m (f x)

main :: IO ()
main = do
  --putStr . show $ min' (-10) 1000 [0,40,20,-12,22,-20,-30,9,3,11,2]
  putStr . show $ rmap 100 id [0,40,20,-12,22,-20,-30,9,3,11,2]
