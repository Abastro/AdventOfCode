module Y2021.Prob01 ( sol1F, sol1S ) where
import Common

sol1F, sol1S :: [Int] -> Int
sol1F l = count True $ zipWith (<) l (tail l)
sol1S l = sol1F $ zipWith3 (\a b c -> a + b + c) l (tail l) (tail $ tail l)