module Y2020.Prob23 ( sol1, sol2 ) where

import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import Common ( applyN )

simulate :: Int -> Int -> M.IntMap Int -> M.IntMap Int
simulate num start snxt = snd $ applyN num step (start, snxt) where
  size = M.size snxt
  step (cur, nxt) = let
    (sel, next:_) = splitAt 3 $ tail $ iterate (nxt M.!) cur
    dest = until (not . (`elem` cur:sel)) ((`mod` size) . pred) cur
    in cur `seq` (next, foldr (uncurry M.insert) nxt
      [(cur, next), (dest, head sel), (last sel, nxt M.! dest)])

sol1 :: [Char] -> [Char]
sol1 inp = let
  cups = pred . read . pure <$> inp  -- To begin with 0
  cupLink = M.fromList $ zip cups (tail $ cycle cups)
  resLink = simulate 100 (head cups) cupLink
  in concat $ show . succ <$> (tail . take (length cups) $ iterate (resLink M.!) 0)

sol2 :: [Char] -> Int
sol2 inp = let
  len = 1000000; cups = pred . read . pure <$> inp
  cupLink = M.unions [M.fromList $ zip cups (tail cups)
    , M.fromList [(pred len, head cups), (last cups, length cups)]
    , M.fromSet succ $ S.fromAscList [0..pred len]]
  resLink = simulate 10000000 (head cups) cupLink
  in product $ succ <$> (take 2 . tail $ iterate (resLink M.!) 0)