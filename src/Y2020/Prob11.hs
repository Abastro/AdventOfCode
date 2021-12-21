module Y2020.Prob11 ( solP11F, solP11S ) where

import Data.Word ( Word8 )
import Data.List ( find )
import Data.Maybe ( mapMaybe )
import qualified Data.Vector.Unboxed as V
import Common ( count, c2word, inRange )

encode :: Int -> Int -> Int -> Int
encode width x y = x + y * width

sol :: Int -> (Int -> [Int]) -> V.Vector Word8 -> Int
sol acc nb seats = count (c2word '#') . V.toList . snd
  $ until (uncurry (==)) (\(_, next) -> next `seq` (next, step next))
  (seats, step seats) where
    step seat = (`V.imap` seat) $ next . count (c2word '#') . map (seat V.!) . nb
    next c u | u == c2word 'L' = if c == 0 then c2word '#' else c2word 'L'
             | u == c2word '#' = if c >= acc then c2word 'L' else c2word '#'
    next _ u = u

solP11F :: [[Char]] -> Int
solP11F inp = sol 5 adj seats where
  seats = V.fromList . fmap c2word . concat $ inp
  width = length $ head inp; height = length inp
  adj p = let i = p `mod` width; j = p `div` width in
    encode width <$> filter (inRange (0, width)) [i-1, i, i+1] <*> filter (inRange (0, height)) [j-1, j, j+1]

solP11S :: [[Char]] -> Int
solP11S inp = sol 6 adj seats where
  seats = V.fromList . fmap c2word . concat $ inp
  width = length $ head inp; height = length inp
  ranges f i = map (takeWhile f . tail) [iterate pred i, repeat i, iterate succ i]
  adj p = mapMaybe (find ((/= c2word '.') . (seats V.!))) $ zipWith (encode width)
    <$> ranges (inRange (0, width)) (p `mod` width) <*> ranges (inRange (0, height)) (p `div` width)