module Y2021.Prob20 ( sol20 ) where
import Common
import qualified Data.Vector.Unboxed as UV

type Image = (Int, Framed UV.Vector Int)
asBit c = if c == '#' then 1 else 0
mkImg l = mkFramed $ map asBit <$> l
bitAt (b, img) p = if inFrame (frame img) p then getAt img p else b

enhance :: UV.Vector Int -> Image -> Image -- TODO Optimize process
enhance ref img@(bg, Framed (Frame w h) _) = (newBg, mkFramedF pxOf (Frame (w+2) (h+2))) where
  pxOf (Vec2 x y) = ref UV.! fromDigit 2 [bitAt img (Vec2 x' y') | y' <- [y-2 .. y], x' <- [x-2 .. x]]
  newBg = ref UV.! fromDigit 2 (replicate 9 bg)

sol20 :: (Int, [String]) -> Int
sol20 (n, l) = length . filter (== 1) . UV.toList $ umap enhanced where
  r : _ : m = l;  ref = UV.fromList $ asBit <$> r
  (_, enhanced) = applyN n (enhance ref) (0, mkImg m)