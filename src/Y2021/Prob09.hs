module Y2021.Prob09 ( sol9F, sol9S ) where
import Data.Char
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import qualified Data.Vector.Unboxed as UV

data HMap = HMap { width :: !Int, height :: !Int, hmap :: !(UV.Vector Int) }
mkHMap l = HMap { width = length $ head l, height = length l, hmap = UV.fromList $ map digit $ concat l }
  where digit c = ord c - ord '0'
getHPos (HMap w _ m) (x, y) = m UV.! (x + y * w)
allCrd hmap = (,) <$> [0 .. pred $ width hmap] <*> [0 .. pred $ height hmap]
nbs (HMap w h _) (x, y) = filter inRange [(pred x, y), (succ x, y), (x, pred y), (x, succ y)] where
  inRange (x, y) = x >= 0 && x < w && y >= 0 && y < h
lows hmap = filter isLow $ allCrd hmap where
  isLow c = getHPos hmap c < minimum (getHPos hmap <$> nbs hmap c)

sol9F :: [String] -> Int
sol9F l = sum $ map (succ . getHPos hmap) (lows hmap) where hmap = mkHMap l

sol9S :: [String] -> Int
sol9S l = product . take 3 $ sortOn Down histo where
  hmap = mkHMap l; lowMap = M.fromList $ zip (lows hmap) [0..]
  markOf p = if getHPos hmap p == 9 then (-1)
    else fromMaybe fromNb $ lowMap M.!? p where
      fromNb = curMap M.! (head . sortOn (getHPos hmap) $ nbs hmap p)
  curMap = M.fromSet markOf $ S.fromList (allCrd hmap)
  histo = M.elems $ M.fromListWith (+) [(v, 1) | v <- M.elems curMap, v /= -1]
