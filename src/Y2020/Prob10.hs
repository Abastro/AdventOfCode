module Y2020.Prob10 where

import Data.List ( sort, foldl' )
import qualified Data.IntMap as M

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

lookOrZ :: Int -> M.IntMap Int -> Int
lookOrZ = M.findWithDefault 0

sol1 :: [Int] -> Int
sol1 inp = let diffs = zipWith (-) (sort inp) (0 : sort inp) in
  count 1 diffs * (count 3 diffs + 1) -- last adapter has +3

sol2 :: [Int] -> Int
sol2 inp = let lookBack m i = lookOrZ (i-1) m + lookOrZ (i-2) m + lookOrZ (i-3) m in
  (M.! maximum inp)
  $ foldl' (\m i -> M.insert i (lookBack m i) m) (M.singleton 0 1)
  $ sort inp

