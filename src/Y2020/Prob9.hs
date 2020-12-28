-- Apparently, this would not work on 32 bit machines
module Y2020.Prob9 where

import Data.Maybe ( fromJust, isJust )
import Data.List ( find, tails, inits )
import qualified Data.IntSet as S
import qualified Data.IntMap as M


isPairAdd :: S.IntSet -> Int -> Bool
isPairAdd set sus = not . S.null $ S.intersection set $ S.map (sus -) set

sol1 :: [Int] -> Int
sol1 inp = let pred (is, j) = isPairAdd (S.fromList is) j in
  snd . fromJust $ find (not . pred) $ fmap head . splitAt 25 <$> tails inp

sol2 :: [Int] -> Int
sol2 inp = let
    val = sol1 inp
    among = fromJust . fromJust $ find isJust
      $ map (\(m, s) -> m M.!? (s - val)) -- Does it sum?
      $ scanl (\(m, _) (s, l) -> (M.insert s l m, s)) (M.empty, 0)
      $ zip (sum <$> inits inp) (tails inp) -- Lists the splits
    desired = fmap snd <$> takeWhile ((/= val). fst)
      $ zip (sum <$> inits among) among
  in minimum desired + maximum desired

