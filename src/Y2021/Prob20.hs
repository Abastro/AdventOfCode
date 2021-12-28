module Y2021.Prob20 ( sol20 ) where
import Common
import qualified Data.Vector.Unboxed as UV
import Data.Foldable

data Image = Image { width :: !Int, height :: !Int, bg :: Int, img :: !(UV.Vector Int) }
toBitVec l = UV.fromList [ if c == '#' then 1 else 0 | c <- l ]
mkImg l = Image { width = length $ head l, height = length l, bg = 0, img = toBitVec $ concat l }
bitAt (Image w h b m) (x, y) = if x >= 0 && x < w && y >= 0 && y < h then m UV.! (x + y * w) else b
fromBits = foldl' (\n d -> n * 2 + d) 0

enhance :: UV.Vector Int -> Image -> Image -- TODO Optimize process
enhance ref image@(Image w h b _) = Image (w+2) (h+2) newBg $ UV.fromList [pxOf x y | y <- [0 .. h+1], x <- [0 .. w+1]] where
  pxOf x y = ref UV.! fromBits [image `bitAt` (x', y') | y' <- [y-2 .. y], x' <- [x-2 .. x]]
  newBg = ref UV.! fromBits (replicate 9 b)

sol20 :: (Int, [String]) -> Int
sol20 (n, l) = length . filter (== 1) . UV.toList $ img enhanced where
  r : _ : m = l;  ref = toBitVec r; image = mkImg m
  enhanced = applyN n (enhance ref) image