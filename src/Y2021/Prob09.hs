module Y2021.Prob09 ( sol9F, sol9S ) where
import Common
import Data.Char
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import qualified Data.Vector.Unboxed as UV

mkHMap :: [[Char]] -> Framed UV.Vector Int
mkHMap l = mkFramed $ map digitToInt <$> l
nbs frame (Vec2 x y) = filter (inFrame frame) [Vec2 (pred x) y, Vec2 (succ x) y, Vec2 x (pred y), Vec2 x (succ y)]
lows hmap = filter isLow $ frameCrds (frame hmap)  where
  isLow c = getAt hmap c < minimum (getAt hmap <$> nbs (frame hmap) c)

sol9F :: [String] -> Int
sol9F l = sum $ map (succ . getAt hmap) (lows hmap) where hmap = mkHMap l

sol9S :: [String] -> Int
sol9S l = product . take 3 $ sortOn Down histo where
  hmap = mkHMap l
  lowMap = M.fromList $ zip (lows hmap) [0..]
  markOf p = if getAt hmap p == 9 then (-1)
    else fromMaybe fromNb $ lowMap M.!? p where
      fromNb = curMap M.! (head . sortOn (getAt hmap) $ nbs (frame hmap) p)
  curMap = M.fromSet markOf $ S.fromList (frameCrds $ frame hmap)
  histo = M.elems $ M.fromListWith (+) [(v, 1) | v <- M.elems curMap, v /= -1]
