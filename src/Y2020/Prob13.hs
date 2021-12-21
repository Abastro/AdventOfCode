{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2020.Prob13 ( solP13F, solP13S ) where

import Data.Maybe ( catMaybes )
import Data.Foldable ( minimumBy )
import Data.Function ( on )
import Text.Read ( readMaybe )
import Common ( deintercalate )

-- findInv n n' finds M: n'M = 1 (mod n) (*Coprime is checked)
findInv :: Int -> Int -> Int
findInv n n' = snd . fst $ curry exec (n', 1) (n, 0) where
  step (r,s) (r',s') = let q = r `div` r' in (r - q * r', s - q * s')
  exec = until ((== 0) . fst . snd) $ \(p, q) -> (q, step p q)

readInput :: [String] -> (Int, [Maybe Int])
readInput [i, js] = (read i, map readMaybe $ deintercalate ',' js)

solP13F :: [String] -> Int
solP13F inp = uncurry (*) $ minimumBy (compare `on` fst) $ zip offset list where
  (start, list) = catMaybes <$> readInput inp
  offset = map ((-start) `mod`) list

solP13S :: [String] -> Int
solP13S inp = (`mod` total) . sum $ map part sch where
  sch = catMaybes $ zipWith (curry sequenceA) [0..] $ snd $ readInput inp
  total = product $ map snd sch
  part (r, n) = let n' = total `div` n in (-r) * n' * findInv n n'