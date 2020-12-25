module Y2020.Prob5 where

import qualified Data.IntSet as S

digitInt :: (Char -> Bool) -> String -> Int
digitInt pred str = sum
  $ zipWith (\i k -> if pred i then k else 0) (reverse str)
  $ iterate (*2) 1

seats :: [String] -> [Int]
seats = map (digitInt (\c -> c == 'B' || c == 'R'))

sol1 :: [String] -> Int
sol1 = maximum . seats

sol2 :: [String] -> Int
sol2 inp = let set = S.fromList $ seats inp in
  head $ dropWhile (`S.member` set) [S.findMin set..]
