module Y2020.Prob17 ( solP17 ) where

import Data.Int ( Int8 )
import Data.Foldable ( Foldable(..) )
import Data.Hashable ( Hashable(..) )
import qualified Data.Vector.Unboxed as V
import qualified Data.HashSet as S
import Common ( count, applyN )

newtype Pos = Pos { getV :: V.Vector Int8 } deriving Eq
instance Hashable Pos where
  hashWithSalt s = V.foldl' hashWithSalt s . getV; {-# INLINE hashWithSalt #-}

process :: Int -> [Pos] -> [Pos]
process dim st = filter next $ Pos <$> V.generateM dim (\i -> [nMin i..nMax i]) where
  nMax i = succ $ foldl' (flip $ max . (V.! i) . getV) minBound st
  nMin i = pred $ foldl' (flip $ min . (V.! i) . getV) maxBound st
  set = S.fromList st;    nbs = V.replicateM dim [-1, 0, 1]
  adjCnt (Pos v) = count True $ (`S.member` set) . Pos . V.imap ((+) . (v V.!)) <$> nbs
  next pos
    | pos `S.member` set = (||) <$> (== 3) <*> (== 4) $ adjCnt pos -- One more from # itself
    | otherwise = (== 3) $ adjCnt pos

solP17 :: (Int, [[Char]]) -> Int
solP17 (dim, inp) = length $ applyN 6 (process dim) input where
  input = do
    (i, line) <- zip [0..] inp; (j, '#') <- zip [0..] line
    pure . Pos $ V.replicate dim 0 V.// [(0, i), (1, j)]