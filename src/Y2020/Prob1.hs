module Y2020.Prob1 where

import Control.Applicative ( Applicative(..) )

import qualified Data.IntSet as S

data Triple = Triple {
  tfst :: Int
  , tsnd :: Int
  , ttrd :: Int
}

findPair :: Int -> [Int] -> [(Int, Int)]
findPair sum list = let set = S.fromList list
  in filter (flip S.member set . snd) $ map (\n -> (n, sum - n)) list

findTriple :: Int -> [Int] -> [Triple]
findTriple sum list = let set = S.fromList list
  in filter (flip S.member set . ttrd) $ liftA2 (\n m -> Triple n m (sum - n - m)) list list

sol1 sum list = uncurry (*) . head $ findPair sum list
sol2 sum list = (\(Triple a b c) -> a * b * c) . head $ findTriple sum list
