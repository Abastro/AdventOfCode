module Y2021.Prob05 ( sol5F, sol5S ) where
import Common
import Data.List
import Text.Read
import qualified Data.IntSet as IS

data Point = Point { x :: !Int, y :: !Int } deriving (Eq, Ord)
instance Read Point where readPrec = Point <$> readPrec <*> (do Punc "," <- lexP; readPrec)
data Line = Line !Point !Point
instance Read Line where readPrec = Line <$> readPrec <*> (do Punc "->" <- lexP; readPrec)
zipInt i j = i + j * 1024
setOf (Line p q) = IS.fromList $ zipWith zipInt (x p ... x q) (y p ... y q)

sol5 :: [Line] -> Int
sol5 = IS.size . snd . foldl' acc (IS.empty, IS.empty) . fmap setOf where -- TODO Inefficient, perhaps better algorithm?
  acc (al, dup) s = (al <> s, dup <> (al `IS.intersection` s)) -- Takes that much

sol5F, sol5S :: [String] -> Int
sol5F l = sol5 [Line p q | Line p q <- map read l, x p == x q || y p == y q]
sol5S l = sol5 $ map read l