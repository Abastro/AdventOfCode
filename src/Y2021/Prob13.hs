module Y2021.Prob13 ( sol13F, sol13S ) where
import Common
import Text.Read
import Data.Char
import Data.List
import qualified Data.Set as S

data Axis = X | Y deriving Read
data Point = Point { xCrd :: !Int, yCrd :: !Int } deriving (Eq, Ord)
data Instr = Instr !Axis !Int
instance Read Point where readPrec = Point <$> readPrec <*> (do Punc "," <- lexP; readPrec)
instance Read Instr where readPrec = Instr <$> (do Ident "FOLD" <- lexP; Ident "ALONG" <- lexP; readPrec) <*> (do Punc "=" <- lexP; readPrec)
readInp l = (S.fromList $ read @Point <$> ps, read @Instr . map toUpper <$> as) where
  [ps, as] = deintercalate [] l
invert n a = n - abs (a - n)
foldTo (Instr X n) (Point x y) = Point (invert n x) y
foldTo (Instr Y n) (Point x y) = Point x (invert n y)

sol13F :: [String] -> Int
sol13F l = S.size $ S.map (foldTo ins) pts where (pts, ins : _) = readInp l

sol13S :: [String] -> String
sol13S l = unlines disp where
  (pts, insts) = readInp l; resPts = foldl' (flip $ S.map . foldTo) pts insts
  maxOf f = maximum $ f <$> S.toList resPts
  disp = [[ if Point x y `S.member` resPts then '#' else '.' | x <- [0 .. maxOf xCrd] ] | y <- [0 .. maxOf yCrd]]