module Y2020.Prob15 ( solP15 ) where

import qualified Data.IntMap as M
import Common ( deintercalate, applyN )

-- Sacrificed performance hugely for shorter code
solP15 :: (Int, String) -> Int
solP15 (ind, inp) = res where
  nums = read <$> deintercalate ',' inp
  step (i, j, m) = (i+1, maybe 0 (i -) (m M.!? j), M.insert j i m)
  initial = (length nums, last nums, M.fromList $ zip (init nums) [1..])
  (_, res, _) = applyN (ind - length nums) step initial