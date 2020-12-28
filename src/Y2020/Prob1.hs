module Y2020.Prob1 where

import qualified Data.IntSet as S

data Triple = Triple {
  tfst :: Int
  , tsnd :: Int
  , ttrd :: Int
}

findPair :: Int -> [Int] -> [(Int, Int)]
findPair sum list = let set = S.fromList list
  in filter (flip S.member set . snd) $ (\n -> (n, sum - n)) <$> list

findTriple :: Int -> [Int] -> [Triple]
findTriple sum list = let set = S.fromList list
  in filter (flip S.member set . ttrd) $ (\n m -> Triple n m (sum - n - m)) <$> list <*> list

sol1 :: Int -> [Int] -> Int
sol1 sum list = uncurry (*) . head $ findPair sum list

sol2 :: Int -> [Int] -> Int
sol2 sum list = (\(Triple a b c) -> a * b * c) . head $ findTriple sum list
