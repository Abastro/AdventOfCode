module Y2020.Prob15 ( sol ) where

import qualified Data.IntMap as M
import Common ( deintercalate )

sol :: Int -> String -> Int
sol ind inp = let
    nums = read <$> deintercalate ',' inp
    next i j m = maybe 0 (i -) (m M.!? j)
  in last $ take (ind - length nums + 1) $ map (\(_, j, _) -> j) $ iterate
  (\(i, j, m) -> let n = next i j m in (i+1, n, M.insert j i m))
  (length nums, last nums, M.fromList $ zip (init nums) [1..])
