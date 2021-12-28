module Y2021.Prob21 ( sol21F, sol21S ) where
import Data.Char
import Control.Monad
import Data.Tuple
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Map.Lazy as ML

readInp :: [String] -> (Int, Int)
readInp [p1, p2] = (digitToInt $ last p1, digitToInt $ last p2); readInp _ = undefined
wrap10 = succ . (`mod` 10) . pred
multTup n (x, y) = (n * x, n * y);  addTup (x, y) (x', y') = (x + x', y + y')

sol21F :: [String] -> Int
sol21F l = 3 * if win1 < win2
  then (scores p2 p2Move !! pred win1) * (win1 + pred win1)
  else (scores p1 p1Move !! win2) * (win2 + win2) where
  -- Moves on each turn, modulo 10
  p1Move = [6,4..]; p2Move = [5,3..]; (p1, p2) = readInp l
  poses p = fmap wrap10 . scanl (+) p;  scores p = scanl (+) 0 . tail . poses p
  turns p mvs = length $ takeWhile (<1000) $ scores p mvs
  win1 = turns p1 p1Move; win2 = turns p2 p2Move

sol21S :: [String] -> Int
sol21S l = snd $ univOf ((pos1, pos2), (21, 21)) where
  (pos1, pos2) = readInp l
  dices = IM.toList . IM.fromListWith (+) $ (, 1) . sum <$> replicateM 3 [1, 2, 3]
  univs = ML.fromSet univOf
    $ S.fromList [((p1, p2), (rem1, rem2)) | p1 <- [1..10], p2 <- [1..10], rem1 <- [1..21], rem2 <- [1..21]]
  -- Win/Lose universes, p1 first
  univOf ((p1, p2), (rem1, rem2)) = foldl' addTup (0, 0) $ do
    (n, occ) <- dices; let p1' = wrap10 (p1 + n); rem1' = rem1 - p1'
    pure . multTup occ $ if rem1' <= 0 then (1, 0) else swap $ univs ML.! ((p2, p1'), (rem2, rem1'))