module Y2021.Prob11 ( sol11F, sol11S ) where
import Common
import Data.Tuple
import Data.Char
import Data.Maybe
import Data.Foldable
import Control.Monad.ST
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV

data Octo s = Octo { width :: !Int, height :: !Int, octos :: !(MV.MVector s Int) }
mkOcto l = do ref <- UV.thaw (UV.fromList $ map digit $ concat l ); pure $ Octo { width = length $ head l, height = length l, octos = ref }
  where digit c = ord c - ord '0'
crdOf (Octo w _ _) (x, y) = x + y * w
unCrd (Octo w _ _) i = swap $ i `divMod` w
nbsI octo = map (crdOf octo) . nbs octo . unCrd octo
nbs (Octo w h _) (x, y) = [(x', y') | x' <- [pred x, x, succ x], y' <- [pred y, y, succ y], inRange (x', y')] where
  inRange (x, y) = x >= 0 && x < w && y >= 0 && y < h

next :: Octo s -> ST s Int
next octo = do
  fl <- traverse incrNFlash [0 .. pred $ MV.length ref]
  flashed <- flashCrds IS.empty (IS.fromList $ catMaybes fl)
  traverse_ (\i -> MV.write ref i 0) flashed
  pure $ length flashed
  where
    ref = octos octo
    incrNFlash i = do t <- MV.read ref i; MV.write ref i (succ t); pure (i <$ boolToMaybe (t >= 9))
    flashCrds olds news = do
      let nnbs = IS.toList news >>= nbsI octo
      fl <- traverse incrNFlash nnbs
      let olds' = olds <> news -- Only tracks new ones
      let news' = IS.fromList (catMaybes fl) IS.\\ olds'
      if IS.null news' then pure $ IS.toList olds' else flashCrds olds' news'

sol11F :: [String] -> Int
sol11F l = runST $ do octo <- mkOcto l; sum <$> traverse next (replicate 100 octo)

sol11S :: [String] -> Int
sol11S l = runST $ do octo <- mkOcto l; go octo 1 where
  go octo n = do fl <- next octo; if fl == MV.length (octos octo) then pure n else go octo (succ n)