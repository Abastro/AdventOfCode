module Y2020.Prob25 ( sol ) where

import Data.List ( foldl', find )

sol :: [Int] -> Int
sol keys@[keyCard, keyDoor] = let
  multBy x = (`mod` 20201227) . (* x)
  powers = zip [0 ..] $ iterate (multBy 7) 1
  Just (loopSize, key) = find ((`elem` keys) . snd) powers
  anotherKey = if key /= keyDoor then keyDoor else keyCard
  in foldl' (flip ($)) 1 $ replicate loopSize (multBy anotherKey)