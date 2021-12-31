module Y2021.Prob20 ( sol20 ) where
import Common
import Data.Word
import qualified Data.Vector.Unboxed as UV
import Data.Bits
import Data.Foldable

type Image = (Word16, Framed UV.Vector Word16)
asBit c = if c == '#' then 1 else 0
mkImg l = mkFramed $ map asBit <$> l
bitAt (b, img) p = if inFrame (frame img) p then getAt img p else b
fromBin = foldl' (\n d -> (n `shiftL` 1) .|. d) 0

enhance :: UV.Vector Word16 -> Image -> Image
enhance ref img@(bg, Framed (Frame w h) _) = (newBg, mkFramedF pxOf (Frame (w+2) (h+2))) where
  listing x y = fromIntegral $ fromBin [bitAt img (Vec2 x' y') | y' <- [y-2 .. y], x' <- [x-2 .. x]]
  pxOf (Vec2 x y) = ref UV.! listing x y
  newBg = ref UV.! fromIntegral (fromBin $ replicate 9 bg)

sol20 :: (Int, [String]) -> Int
sol20 (n, l) = length . filter (== 1) . UV.toList $ umap enhanced where
  r : _ : m = l;  ref = UV.fromList $ asBit <$> r
  (_, enhanced) = applyN n (enhance ref) (0, mkImg m)