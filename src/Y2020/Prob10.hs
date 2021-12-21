module Y2020.Prob10 ( solP10F, solP10S ) where

import Data.List ( sort )
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Common ( count )

solP10F :: [Int] -> Int
solP10F inp = let diffs = zipWith (-) (sort inp) (0 : sort inp) in
  count 1 diffs * (count 3 diffs + 1) -- last adapter has +3

solP10S :: [Int] -> Int
solP10S inp = lazyMap M.! maximum inp where
  lookOrZ i = if i /= 0 then M.findWithDefault 0 i else const 1
  lookBack m i = lookOrZ (i-1) m + lookOrZ (i-2) m + lookOrZ (i-3) m
  lazyMap = M.fromSet (lookBack lazyMap) $ S.fromList inp