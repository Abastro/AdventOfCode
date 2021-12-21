module Y2020.Prob3 ( solP3F, solP3S ) where

sol :: Int -> Int -> [[Char]] -> Int
sol moveX moveY inp = length . filter (== '#')
  $ zipWith (!!) sparse $ map ((`mod` width) . (* moveX)) [0..] where
    width = length $ head inp
    sparse = map snd . filter ((== 0) . (`mod` moveY) . fst) $ zip [0..] inp

solP3F :: [[Char]] -> Int
solP3F = sol 3 1

solP3S :: [[Char]] -> Int
solP3S = product . ([sol 1 1, sol 3 1, sol 5 1, sol 7 1, sol 1 2] <*>) . pure
