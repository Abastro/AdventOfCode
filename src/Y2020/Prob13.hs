module Y2020.Prob13 where

import Data.Maybe ( catMaybes )
import Data.Foldable ( minimumBy )

import Text.Read ( readMaybe )

-- findInv n n' finds M: n'M = 1 (mod n) (Checked that n is primes)
findInv :: Int -> Int -> Int
findInv n n' = let
    step :: (Int, Int) -> (Int, Int) -> (Int, Int)
    step (r,s) (r',s') = let q = r `div` r' in (r - q * r', s - q * s')
    exec = until ((== 0) . fst . snd) $ \(p, q) -> (q, step p q)
  in snd . fst $ curry exec (n', 1) (n, 0)

readInput :: [String] -> (Int, [Maybe Int])
readInput [i, js] = (read i, map readMaybe $ words
  $ map (\c -> if c == ',' then ' ' else c) js)

sol1 :: [String] -> Int
sol1 inp = let
    (start, list) = catMaybes <$> readInput inp
    offset = map ((-start) `mod`) list
  in uncurry (*) $ minimumBy (\(x, _) (y, _) -> compare x y) $ zip offset list

sol2 :: [String] -> Int
sol2 inp = let
    sch = catMaybes $ zipWith (curry sequenceA) [0..] $ snd $ readInput inp
    total = product $ map snd sch
    part r n = let n' = total `div` n in (-r) * n' * findInv n n'
  in (`mod` total) . sum $ map (uncurry part) sch

