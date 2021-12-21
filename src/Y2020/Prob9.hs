{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2020.Prob9 ( solP9F, solP9S ) where

import Data.List ( tails )
import qualified Data.IntSet as S
import qualified Data.IntMap as M

solP9F :: [Int] -> Int
solP9F inp = let pred (is, j) = isPairAdd (S.fromList is) j in
  snd . head $ filter (not . pred) $ fmap head . splitAt 25 <$> tails inp
  where isPairAdd set sus = not . S.null $ S.intersection set $ S.map (sus -) set

solP9S :: [Int] -> Int
solP9S inp = minimum desired + maximum desired where
  target = solP9F inp; desired = findRange M.empty 0 inp
  withSum l = zip l $ scanl (+) 0 l
  findRange sums curSum rem@(x:xs) = case sums M.!? (curSum - target) of
    Just found -> fmap fst $ takeWhile ((/= target) . snd) $ withSum found
    Nothing -> findRange (M.insert curSum rem sums) (curSum + x) xs