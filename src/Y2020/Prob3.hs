module Y2020.Prob3 ( sol1, sol2 ) where

sol1 :: Int -> Int -> [[Char]] -> Int
sol1 moveX moveY inp = length . filter (== '#')
  $ zipWith (!!) sparse $ map ((`mod` width) . (* moveX)) [0..] where
    width = length $ head inp
    sparse = map snd . filter ((== 0) . (`mod` moveY) . fst) $ zip [0..] inp

sol2 :: [[Char]] -> Int
sol2 = product . ([sol1 1 1, sol1 3 1, sol1 5 1, sol1 7 1, sol1 1 2] <*>) . pure
