-- |Suboptimal approach by myself; Cube coords were less duplicate than expected
module Y2021.Prob22b ( sol22F, sol22S ) where
import Common
import Data.Char
import Data.List
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

data Cmd = Cmd !Bool !(Vec3 Int) !(Vec3 Int) deriving Show
readCmd :: String -> Cmd -- Repr. region p1 <= X < p2
readCmd s = Cmd (flag == "on") (Vec3 x1 y1 z1) (Vec3 (succ x2) (succ y2) (succ z2)) where
  [flag, cuboid] = words s
  [x1, x2, y1, y2, z1, z2] = map (read @Int) . words $ map filt cuboid
  filt c = if isDigit c || c == '-' then c else ' '

mkBimap cmds axis = (cmap, cvec) where
  crds = IS.toAscList . IS.fromList $ [axis p | Cmd _ p1 p2 <- cmds, p <- [p1, p2]]
  cmap = IM.fromAscList $ zip crds [0..]; cvec = UV.fromList crds

-- |For specific i,j
lenZ :: V.Vector Cmd -> [Int] -> Int
lenZ cmds idxs = sum . map lenZOf . IS.toList $ foldl' calc IS.empty cs where
  cs = [cmds V.! t | t <- idxs]; (mZ, vZ) = mkBimap cs sz
  segZ (Cmd _ p1 p2) = [mZ IM.! sz p1 .. pred $ mZ IM.! sz p2]
  lenZOf k = vZ UV.! succ k - vZ UV.! k
  calc ons cmd@(Cmd flag _ _) = (if flag then (ons <>) else (ons IS.\\)) $ IS.fromAscList (segZ cmd)

handleAxis :: V.Vector Cmd -> (Vec3 Int -> Int) -> (V.Vector Cmd -> [Int] -> Int) -> [Int] -> Int
handleAxis cmds axis f idxs = sum . map unitSize . IM.toList $ foldl' go IM.empty idxs where
  (cm, cv) = mkBimap [cmds V.! t | t <- idxs] axis
  segment (Cmd _ p1 p2) = [cm IM.! axis p1 .. pred $ cm IM.! axis p2]
  unitSize (i, ts) = (cv UV.! succ i - cv UV.! i) * f cmds ts
  go pre t = flip (IM.unionWith (<>)) pre $ IM.fromAscList [(i, [t]) | i <- segment (cmds V.! t)]

sol22 :: V.Vector Cmd -> Int
sol22 cmds = handleAxis cmds sx areaYZ [0 .. pred $ V.length cmds] where
  areaYZ cmds = handleAxis cmds sy lenZ -- For specific i

sol22F, sol22S :: [String] -> Int
sol22F l = sol22 . V.fromList $ cmds where
  cmds = [cmd | cmd@(Cmd _ (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)) <- readCmd <$> l,
    x1 >= -50, y1 >= -50, z1 >= -50, x2 <= 50, y2 <= 50, z2 <= 50]
sol22S l = sol22 . V.fromList $ readCmd <$> l