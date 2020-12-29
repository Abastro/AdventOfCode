module Y2020.Prob5 ( sol1, sol2 ) where

import Data.Bits ( Bits(..) )
import qualified Data.IntSet as S

seats :: [[Char]] -> [Int]
seats = map $ digitInt (`elem` ['B', 'R']) where
  digitInt pred str = sum
    $ zipWith (\i k -> if pred i then k else 0) (reverse str)
    $ iterate (`shift` 1) 1

sol1 :: [String] -> Int
sol1 = maximum . seats

sol2 :: [String] -> Int
sol2 inp = let set = S.fromList $ seats inp in
  head $ dropWhile (`S.member` set) [S.findMin set..]
