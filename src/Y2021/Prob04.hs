{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2021.Prob04 ( sol4F, sol4S ) where
import Common
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS

rows = [1..5]
bingos = S.fromList <$> [(x, ) <$> rows | x <- rows] <> [(, y) <$> rows | y <- rows]
founds board = scanl (\s c -> maybe s (`S.insert` s) $ board IM.!? c) S.empty -- Starts from empty
isBingo found = any (`S.isSubsetOf` found) bingos
fstBingo calls board = length . takeWhile (not . isBingo) $ founds board calls
score calls board = (sum . IS.toList $ IM.keysSet board IS.\\ IS.fromList called) * last called where called = take (fstBingo calls board) calls

readBoard :: [String] -> IM.IntMap (Int, Int)
readBoard ls = IM.fromList [(read n, (x, y)) | (x, l) <- zip rows ls, (y, n) <- zip rows $ words l]

sol4 :: (Int -> Int) -> [String] -> Int
sol4 f (cs : _ : bs) = score calls . head $ sortOn (f . fstBingo calls) boards where
  calls = read @Int <$> deintercalate ',' cs
  boards = readBoard <$> deintercalate "" bs

sol4F, sol4S :: [String] -> Int
sol4F = sol4 id; sol4S = sol4 negate