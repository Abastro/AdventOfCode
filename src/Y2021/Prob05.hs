module Y2021.Prob05 ( sol5F, sol5S ) where
import Data.List
import Text.Read
import qualified Data.Set as S

data Point = Point{ x :: !Int, y :: !Int } deriving (Eq, Ord)
instance Read Point where readPrec = Point <$> readPrec <*> (do Punc "," <- lexP; readPrec)
data Line = Line !Point !Point
instance Read Line where readPrec = Line <$> readPrec <*> (do Punc "->" <- lexP; readPrec)
x ... y = case x `compare` y of LT -> [x, succ x .. y]; EQ -> repeat x; GT -> [x, pred x .. y]
setOf (Line p q) = S.fromList $ zipWith Point (x p ... x q) (y p ... y q)

sol5 :: [Line] -> Int
sol5 = S.size . snd . foldl' acc (S.empty, S.empty) . fmap setOf where -- Inefficient, perhaps better algorithm?
  acc (al, dup) s = (al <> s, dup <> (al `S.intersection` s))

sol5F, sol5S :: [String] -> Int
sol5F l = sol5 [Line p q | Line p q <- map read l, x p == x q || y p == y q]
sol5S l = sol5 $ map read l