module Y2021.Prob15 ( sol15F, sol15S ) where
import Common
import Data.Char
import Data.Word
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Set as S
import Control.Monad
import Control.Monad.ST

mkRisks :: [[Char]] -> Framed UV.Vector Word8
mkRisks l = mkFramed $ map (fromIntegral . digitToInt) <$> l
nbs frame i = [frameIdx frame v | v <- [Vec2 (pred x) y, Vec2 (succ x) y, Vec2 x (pred y), Vec2 x (succ y)], inFrame frame v]
  where Vec2 x y = fromIdx frame i
data WRisk = WRisk !Word16 !Int deriving (Eq, Ord)

calcRisk :: Framed UV.Vector Word8 -> MV.MVector s Word16 -> MV.MVector s Bool -> S.Set WRisk -> WRisk -> ST s ()
calcRisk rmap@Framed { frame = fr } risk unvisit toVisit (WRisk curRisk cur) = do
  nexts <- filterM (MV.read unvisit) $ nbs fr cur
  MV.write unvisit cur False
  toVisit' <- foldM modRisk toVisit nexts
  let handleMin pq = case S.minView pq of
        Nothing -> pure ()
        Just (nxt@(WRisk _ minV), rem) -> do
          unvis <- MV.read unvisit minV
          if not unvis then handleMin rem else calcRisk rmap risk unvisit rem nxt
  handleMin toVisit'
  where
    modRisk set i = do
      old <- MV.read risk i
      let new = curRisk + fromIntegral (umap rmap UV.! i)
      if new < old then MV.write risk i new >> pure (S.insert (WRisk new i) set) else pure set

getRisk :: Framed UV.Vector Word8 -> Int
getRisk rmap = runST $ do
  let l = UV.length $ umap rmap
  risk <- MV.replicate l (maxBound @Word16)
  unvisit <- MV.replicate l True
  calcRisk rmap risk unvisit mempty (WRisk 0 0)
  fmap fromIntegral $ MV.read risk $ pred l

sol15F :: [String] -> Int
sol15F l = getRisk $ mkRisks l

sol15S :: [String] -> Int
sol15S l = getRisk $ Framed (Frame (w * 5) (h * 5)) $ UV.fromList large where
  rmap@Framed{ frame = Frame w h } = mkRisks l
  large = [let v = getAt rmap (Vec2 i j) + s + t in succ (pred v `mod` 9)
    | t <- [0..4], j <- [0 .. pred h], s <- [0..4], i <- [0 .. pred w]]