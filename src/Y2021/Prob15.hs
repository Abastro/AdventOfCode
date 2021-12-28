module Y2021.Prob15 ( sol15F, sol15S ) where
import Data.Char
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Set as S
import Control.Monad
import Control.Monad.ST

data HMap = HMap { width :: !Int, height :: !Int, hmap :: !(UV.Vector Int) }
mkHMap l = HMap { width = length $ head l, height = length l, hmap = UV.fromList $ map digit $ concat l }
  where digit c = ord c - ord '0'
crdOf (HMap w _ _) (x, y) = x + y * w
nbs (HMap w h _) (x, y) = filter inRange [(pred x, y), (succ x, y), (x, pred y), (x, succ y)] where
  inRange (x, y) = x >= 0 && x < w && y >= 0 && y < h

riskTo :: HMap -> MV.MVector s Int -> MV.MVector s Bool -> S.Set (Int, (Int, Int)) -> (Int, (Int, Int)) -> ST s ()
riskTo hm risk unvisit toVisit (curRisk, cur) = do -- TODO Refactor & Optimize
  nexts <- filterM (MV.read unvisit . crdOf hm) $ nbs hm cur
  MV.write unvisit (crdOf hm cur) False
  let modRisk i = do old <- MV.read risk i
                     let new = min (curRisk + hmap hm UV.! i) old in MV.write risk i new >> pure (old, new)
  nextRes <- traverse (\v -> (v, ) <$> modRisk (crdOf hm v)) nexts
  let toVisit' = foldl' (flip S.delete) toVisit [(old, p) | (p, (old, _)) <- nextRes]
  let toVisit'' = foldl' (flip S.insert) toVisit' [(new, p) | (p, (_, new)) <- nextRes]
  case S.minView toVisit'' of
    Nothing -> pure ()
    Just (nextV, nextVisit) -> riskTo hm risk unvisit nextVisit nextV

getRisk :: HMap -> Int
getRisk hmap@(HMap _ _ m) = runST $ do
  risk <- MV.replicate (UV.length m) (maxBound @Int)
  unvisit <- MV.replicate (UV.length m) True
  riskTo hmap risk unvisit S.empty (0, (0, 0))
  MV.read risk $ pred (UV.length m)

sol15F :: [String] -> Int
sol15F l = getRisk $ mkHMap l

sol15S :: [String] -> Int
sol15S l = getRisk $ HMap (w * 5) (h * 5) (UV.fromList $ newAt <$> [0 .. pred $ 5 * h] <*> [0 .. pred $ 5 * w]) where
  hmap@(HMap w h m) = mkHMap l
  newAt x y = succ (pred v `mod` 9) where (r, t) = x `divMod` w; (s, u) = y `divMod` h; v = (m UV.! crdOf hmap (t, u)) + r + s