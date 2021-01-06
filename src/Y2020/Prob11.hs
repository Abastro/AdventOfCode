module Y2020.Prob11 ( sol1, sol2 ) where

import Data.Word ( Word8 )
import Data.List ( find )
import Data.Maybe ( mapMaybe )
import qualified Data.Vector.Unboxed as V
import Common ( count, c2word )

data Frame = Frame { validX :: Int -> Bool, validY :: Int -> Bool
  , encode :: Int -> Int -> Int, decodeX :: Int -> Int, decodeY :: Int -> Int }
mkFrame :: [[a]] -> Frame
mkFrame inp = Frame { validX = \x -> x >= 0 && x < width, validY = \y -> y >= 0 && y < height
  , encode = \x y -> x + y * width, decodeX = (`mod` width), decodeY = (`div` width)
} where width = length $ head inp; height = length inp

sol :: Int -> (Int -> [Int]) -> V.Vector Word8 -> Int
sol acc nb seats = count (c2word '#') . V.toList . snd
  $ until (uncurry (==)) (\(_, next) -> next `seq` (next, step next))
  (seats, step seats) where
    step seat = (`V.imap` seat) $ next . count (c2word '#') . map (seat V.!) . nb
    next c u | u == c2word 'L' = if c == 0 then c2word '#' else c2word 'L'
             | u == c2word '#' = if c >= acc then c2word 'L' else c2word '#'
    next _ u = u

sol1 :: [[Char]] -> Int
sol1 inp = sol 5 adj seats where
  seats = V.fromList . fmap c2word . concat $ inp; fr = mkFrame inp
  adj p = let i = decodeX fr p; j = decodeY fr p in
    encode fr <$> filter (validX fr) [i-1, i, i+1] <*> filter (validY fr) [j-1, j, j+1]

sol2 :: [[Char]] -> Int
sol2 inp = sol 6 adj seats where
  seats = V.fromList . fmap c2word . concat $ inp; fr = mkFrame inp
  ranges f i = map (takeWhile f . tail) [iterate pred i, repeat i, iterate succ i]
  adj p = let i = decodeX fr p; j = decodeY fr p in
    mapMaybe (find ((/= c2word '.') . (seats V.!))) $ zipWith (encode fr)
    <$> ranges (validX fr) i <*> ranges (validY fr) j