module Y2021.Prob25 ( sol25F ) where
import Common
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Foldable
import Control.Monad.ST
mkMap l = mkFramed $ map st <$> l where st '>' = 1; st 'v' = 2; st _ = 0

runStep :: Framed (MV.MVector s) Int -> ST s Bool -- TODO More optimization
runStep st@Framed { frame = Frame w h } = do
  ch1 <- anyM (fmap snd . move (flip idx) (pred w) 1) [0 .. pred h]
  ch2 <- anyM (fmap snd . move idx (pred h) 2) [0 .. pred w]
  pure $ ch1 || ch2
  where
    anyM f = foldlM (\b -> fmap (b ||) . f) False
    idx x y = x + y * w
    move ix mx m i = do
      p0 <- MV.read (umap st) (ix i 0)
      foldlM (flip $ moveOn p0) (p0, False) [0 .. mx] where
      moveOn p0 j (cur, b) = do
        let j' = if j == mx then 0 else succ j
        next <- if j == mx then pure p0 else MV.read (umap st) (ix i j')
        if cur == m && next == 0 then do
          MV.write (umap st) (ix i j) 0; MV.write (umap st) (ix i j') m; pure (next, True)
        else pure (next, b)

sol25F :: [String] -> Int
sol25F l = runST $ framedThaw (mkMap l) >>= ites 1 where
  ites i s = do b <- runStep s; if b then ites (succ i) s else pure i