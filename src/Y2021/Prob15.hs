module Y2021.Prob15 ( sol15F, sol15S ) where
import Common
import Data.Char
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.PQueue.Prio.Min as P
import Control.Monad
import Control.Monad.ST

mkRisks :: [[Char]] -> Framed UV.Vector Int
mkRisks l = mkFramed $ map digitToInt <$> l
nbs frame i = [frameIdx frame v | v <- [Vec2 (pred x) y, Vec2 (succ x) y, Vec2 x (pred y), Vec2 x (succ y)], inFrame frame v]
  where Vec2 x y = fromIdx frame i

riskTo :: Framed UV.Vector Int -> MV.MVector s Int -> MV.MVector s Bool -> P.MinPQueue Int Int -> (Int, Int) -> ST s ()
riskTo rmap risk unvisit toVisit (curRisk, cur) = do
  let fr = frame rmap
  nexts <- filterM (MV.read unvisit) $ nbs fr cur
  MV.write unvisit cur False
  let modRisk i = do MV.modify risk (min $ curRisk + umap rmap UV.! i) i
                     (, i) <$> MV.read risk i
  nextRes <- traverse modRisk nexts
  let toVisit' = foldl' (flip $ uncurry P.insert) toVisit nextRes
  let handleMin pq = case P.minViewWithKey pq of
        Nothing -> pure ()
        Just ((minP, minV), rem) -> do
          unvis <- MV.read unvisit minV
          if not unvis then handleMin rem
          else riskTo rmap risk unvisit rem (minP, minV)
  handleMin toVisit'

getRisk :: Framed UV.Vector Int -> Int
getRisk rmap = runST $ do
  let l = UV.length $ umap rmap
  risk <- MV.replicate l (maxBound @Int)
  unvisit <- MV.replicate l True
  riskTo rmap risk unvisit P.empty (0, 0)
  MV.read risk $ pred l

sol15F :: [String] -> Int
sol15F l = getRisk $ mkRisks l

sol15S :: [String] -> Int
sol15S l = getRisk $ Framed (Frame (w * 5) (h * 5)) $ UV.fromList large where
  rmap@Framed{ frame = Frame w h } = mkRisks l
  large = [let v = getAt rmap (Vec2 i j) + s + t in succ (pred v `mod` 9)
    | t <- [0..4], j <- [0 .. pred h], s <- [0..4], i <- [0 .. pred w]]