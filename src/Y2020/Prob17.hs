module Y2020.Prob17 ( sol1, sol2 ) where

import Data.Hashable ( Hashable )
import qualified Data.HashSet as S
import Common ( count )
import GHC.Generics ( Generic )

data Triple = Triple Int Int Int deriving (Eq, Generic)
instance Hashable Triple
neighbor :: Triple -> [Triple]
neighbor (Triple i j k) = Triple
  <$> [i-1, i, i+1] <*> [j-1, j, j+1] <*> [k-1, k, k+1]

data Quad = Quad Int Int Int Int deriving (Eq, Generic)
instance Hashable Quad
neighbor' :: Quad -> [Quad]
neighbor' (Quad i j k l) = Quad
  <$> [i-1, i, i+1] <*> [j-1, j, j+1] <*> [k-1, k, k+1] <*> [l-1, l, l+1]

readInit :: [[Char]] -> S.HashSet (Int, Int)
readInit inp = S.fromList $ do
  (i, line) <- zip [0..] inp; (j, '#') <- zip [0..] line; [(i, j)]

process :: (Eq a, Hashable a) => (a -> [a]) -> S.HashSet a -> S.HashSet a
process nbh st = let
    adj pos = map (`S.member` st) $ nbh pos
    next pos
      | S.member pos st = count True (adj pos) `elem` [3, 4] -- One more from # itself
      | otherwise = count True (adj pos) == 3
  in S.filter next $ foldr1 S.union $ S.map (S.fromList . nbh) st

sol1 :: [[Char]] -> Int
sol1 inp = let
    cube = S.map (uncurry $ Triple 0) $ readInit inp
  in S.size $ iterate (process neighbor) cube !! 6

sol2 :: [[Char]] -> Int
sol2 inp = let
    cube = S.map (uncurry $ Quad 0 0) $ readInit inp
  in S.size $ iterate (process neighbor') cube !! 6


