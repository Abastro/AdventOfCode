module Y2021.Prob11 ( sol11F, sol11S ) where
import Common
import Data.Char
import Data.Maybe
import Data.Foldable
import Control.Monad
import Control.Monad.ST
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed.Mutable as MV

mkOcto l = framedThaw . mkFramed $ map digitToInt <$> l
nbsI frame = map (frameIdx frame) . nbs frame . fromIdx frame
nbs frame (Vec2 x y) = filter (inFrame frame) $ Vec2 <$> [pred x, x, succ x] <*> [pred y, y, succ y]

next :: Framed (MV.MVector s) Int -> ST s Int
next octo = do
  fl <- traverse incrNFlash [0 .. pred $ MV.length ref]
  flashed <- flashCrds IS.empty (IS.fromList $ catMaybes fl)
  traverse_ (\i -> MV.write ref i 0) flashed
  pure $ length flashed
  where
    ref = umap octo
    incrNFlash i = do t <- MV.read ref i; MV.write ref i (succ t)
                      pure (i <$ guard (t >= 9))
    flashCrds olds news = do
      let nnbs = IS.toList news >>= nbsI (frame octo)
      fl <- traverse incrNFlash nnbs
      let olds' = olds <> news -- Only tracks new ones
      let news' = IS.fromList (catMaybes fl) IS.\\ olds'
      if IS.null news' then pure $ IS.toList olds' else flashCrds olds' news'

sol11F :: [String] -> Int
sol11F l = runST $ do octo <- mkOcto l; sum <$> traverse next (replicate 100 octo)

sol11S :: [String] -> Int
sol11S l = runST $ do octo <- mkOcto l; go octo 1 where
  go octo n = do fl <- next octo; if fl == MV.length (umap octo) then pure n else go octo (succ n)