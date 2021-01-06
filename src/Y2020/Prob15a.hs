module Y2020.Prob15a ( sol ) where

import Control.Monad ( zipWithM_, foldM )
import Control.Monad.ST ( ST, runST )
import Data.Word ( Word32 )
import qualified Data.Vector.Unboxed.Mutable as MV
import Common ( deintercalate )

-- Faster version, not so longer
grow :: Int -> MV.MVector s Word32 -> ST s (MV.MVector s Word32)
grow k vec = if k < l then pure vec else do
  vec' <- MV.grow vec l
  MV.slice l l vec' `MV.set` maxBound
  grow k vec'   where l = MV.length vec

sol :: Int -> String -> Int
sol ind inp = runST $ do
  let nums = read <$> deintercalate ',' inp
  v <- MV.replicate (maximum nums) maxBound
  zipWithM_ (MV.write v) (init nums) [1..]
  (_, res, _) <- foldM (flip . const $ step) (fromIntegral $ length nums, last nums, v)
    [1 .. ind - length nums]
  pure $ fromIntegral res where
    step (i, j, v) = do
      v' <- grow j v;   prev <- MV.read v' j;   MV.write v' j i
      pure (i+1, if prev == maxBound then 0 else fromIntegral $ i - prev, v')