module Y2021.Prob03 ( sol3F, sol3S ) where
import Common
import Data.List

isTCommon l = count True l >= count False l
toBin = map (== '1')
fromBin = foldl' @[] (\n d -> n * 2 + if d then 1 else 0) 0

sol3F :: [String] -> Int
sol3F l = fromBin r * fromBin (not <$> r) where r = isTCommon <$> transpose (toBin <$> l)

sol3S :: [String] -> Int
sol3S l = fromBin (go True l') * fromBin (go False l') where
  l' = toBin <$> l; go _ [c] = c
  go f l = (f == flag) : go f [bs | b : bs <- l, f == (b == flag)] where flag = isTCommon $ head <$> l