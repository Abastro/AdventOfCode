module Y2020.Prob13 ( sol1, sol2 ) where

import Data.Maybe ( catMaybes )
import Data.Foldable ( minimumBy )
import Data.Function ( on )
import Text.Read ( readMaybe )
import Common ( deintercalate )

-- findInv n n' finds M: n'M = 1 (mod n) (*Coprime is checked)
findInv :: Int -> Int -> Int
findInv n n' = let
    step (r,s) (r',s') = let q = r `div` r' in (r - q * r', s - q * s')
    exec = until ((== 0) . fst . snd) $ \(p, q) -> (q, step p q)
  in snd . fst $ curry exec (n', 1) (n, 0)

readInput :: [String] -> (Int, [Maybe Int])
readInput [i, js] = (read i, map readMaybe $ deintercalate ',' js)

sol1 :: [String] -> Int
sol1 inp = let
    (start, list) = catMaybes <$> readInput inp
    offset = map ((-start) `mod`) list
  in uncurry (*) $ minimumBy (compare `on` fst) $ zip offset list

sol2 :: [String] -> Int
sol2 inp = let
    sch = catMaybes $ zipWith (curry sequenceA) [0..] $ snd $ readInput inp
    total = product $ map snd sch
    part (r, n) = let n' = total `div` n in (-r) * n' * findInv n n'
  in (`mod` total) . sum $ map part sch

