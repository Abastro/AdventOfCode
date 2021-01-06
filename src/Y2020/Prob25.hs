module Y2020.Prob25 ( sol ) where

import Data.Bifunctor ( bimap )
import Common ( applyN )

sol :: [Int] -> Int
sol [kCard, kDoor] = applyN loopSize (multBy anotherKey) 1 where
  multBy x = (`mod` 20201227) . (* x)
  (loopSize, key) = until (\(_, r) -> r == kCard || r == kDoor) (bimap succ $ multBy 7) (0, 1)
  anotherKey = if key /= kDoor then kDoor else kCard