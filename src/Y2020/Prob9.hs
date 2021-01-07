module Y2020.Prob9 ( sol1, sol2 ) where

import Data.List ( tails )
import qualified Data.IntSet as S
import qualified Data.IntMap as M

sol1 :: [Int] -> Int
sol1 inp = let pred (is, j) = isPairAdd (S.fromList is) j in
  snd . head $ filter (not . pred) $ fmap head . splitAt 25 <$> tails inp
  where isPairAdd set sus = not . S.null $ S.intersection set $ S.map (sus -) set

sol2 :: [Int] -> Int
sol2 inp = minimum desired + maximum desired where
  target = sol1 inp; desired = findRange M.empty 0 inp
  withSum l = zip l $ scanl (+) 0 l
  findRange sums curSum rem@(x:xs) = case sums M.!? (curSum - target) of
    Just found -> fmap fst $ takeWhile ((/= target) . snd) $ withSum found
    Nothing -> findRange (M.insert curSum rem sums) (curSum + x) xs