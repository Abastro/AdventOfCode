module Y2021.Prob22 ( sol22F, sol22S ) where
import Common
import Data.Char
import Data.Foldable
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Vector as V

data Cmd = Cmd !Bool !(Vec3 Int) !(Vec3 Int) deriving Show -- TODO Optimize & Reduce code (many duplicates)
readCmd :: String -> Cmd -- Repr. region p1 <= X < p2
readCmd s = Cmd (flag == "on") (Vec3 x1 y1 z1) (Vec3 (succ x2) (succ y2) (succ z2)) where
  [flag, cuboid] = words s
  [x1, x2, y1, y2, z1, z2] = map (read @Int) . words $ map filt cuboid
  filt c = if isDigit c || c == '-' then c else ' '

mkBimap cmds coord = (cmap, cvec) where
  crds = IS.toAscList $ IS.fromList [coord p | Cmd _ p1 p2 <- cmds, p <- [p1, p2]]
  cmap = IM.fromList $ zip crds [0..]; cvec = V.fromList crds

-- |For specific i,j
lenZ :: V.Vector Cmd -> [Int] -> Int
lenZ cmds idxs = sum . map lenZOf . IS.toList $ foldl' calc IS.empty cs where
  cs = [cmds V.! t | t <- idxs]; (mZ, vZ) = mkBimap cs sz
  segZ (Cmd _ p1 p2) = [mZ IM.! sz p1 .. pred $ mZ IM.! sz p2]
  lenZOf k = vZ V.! succ k - vZ V.! k
  calc ons cmd@(Cmd flag _ _) = foldl' (flip $ if flag then IS.insert else IS.delete) ons $ segZ cmd
-- |For specific i
areaYZ :: V.Vector Cmd -> [Int] -> Int
areaYZ cmds idxs = sum . map (\(j, ts) -> lenYOf j * lenZ cmds ts) . IM.toList $ foldl' calc IM.empty idxs where
  (mY, vY) = mkBimap [cmds V.! t | t <- idxs] sy
  segY (Cmd _ p1 p2) = [mY IM.! sy p1 .. pred $ mY IM.! sy p2]
  lenYOf j = vY V.! succ j - vY V.! j
  calc winm t = foldl' (flip . uncurry $ IM.insertWith (<>)) winm $ [(j, [t]) | j <- segY (cmds V.! t)]

sol22 :: V.Vector Cmd -> Int
sol22 cmds = sum . map (\(i, ts) -> lenXOf i * areaYZ cmds ts) . IM.toList $ foldl' calc IM.empty [0 .. pred $ V.length cmds] where
  (mX, vX) = mkBimap (V.toList cmds) sx
  segX (Cmd _ p1 p2) = [mX IM.! sx p1 .. pred $ mX IM.! sx p2]
  lenXOf i = vX V.! succ i - vX V.! i
  calc winm t = foldl' (flip . uncurry $ IM.insertWith (<>)) winm $ [(i, [t]) | i <- segX (cmds V.! t)]

sol22F, sol22S :: [String] -> Int
sol22F l = sol22 . V.fromList $ cmds where
  cmds = [cmd | cmd@(Cmd _ (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)) <- readCmd <$> l,
    x1 >= -50, y1 >= -50, z1 >= -50, x2 <= 50, y2 <= 50, z2 <= 50]
sol22S l = sol22 . V.fromList $ readCmd <$> l