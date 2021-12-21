module Y2020.Prob5 ( solP5F, solP5S ) where

import Data.Bits ( Bits(..) )
import qualified Data.IntSet as S

seats :: [[Char]] -> [Int]
seats = map $ digitInt (`elem` ['B', 'R']) where
  digitInt pred str = sum
    $ zipWith (\i k -> if pred i then k else 0) (reverse str) $ iterate (`shift` 1) 1

solP5F :: [String] -> Int
solP5F = maximum . seats

solP5S :: [String] -> Int
solP5S inp = let set = S.fromList $ seats inp in
  head $ dropWhile (`S.member` set) [S.findMin set..]
