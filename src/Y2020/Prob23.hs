{-# LANGUAGE BangPatterns #-}
module Y2020.Prob23 ( sol1, sol2 ) where

-- TODO Rewrite in ST Monad; current version performs poorly
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M

step :: Int -> (Int, M.IntMap Int) -> (Int, M.IntMap Int)
step size (!cur, !nxt) = let
  (sel, next:_) = splitAt 3 $ tail $ iterate (nxt M.!) cur
  dest = until (not . (`elem` cur:sel)) ((`mod` size) . pred) cur
  in (next, foldr (uncurry M.insert) nxt
    [(cur, next), (dest, head sel), (last sel, nxt M.! dest)])

sol1 :: [Char] -> [Char]
sol1 inp = let
  -- To begin with 0
  cups = pred . read . pure <$> inp
  cupLink = M.fromList $ zip cups (tail $ cycle cups)
  resLink = snd $ iterate (step $ length cups) (head cups, cupLink) !! 100
  in concat $ show . succ <$> (tail . take (length cups) $ iterate (resLink M.!) 0)

sol2 :: [Char] -> Int
sol2 inp = let
  len = 1000000
  cups = pred . read . pure <$> inp
  cupLink = M.unions [M.fromList $ zip cups (tail cups)
    , M.fromList [(pred len, head cups), (last cups, length cups)]
    , M.fromSet succ $ S.fromAscList [0..pred len]]
  resLink = snd $ foldr (const $ step len) (head cups, cupLink) [1..10000000]
  in product $ succ <$> (take 2 . tail $ iterate (resLink M.!) 0)
