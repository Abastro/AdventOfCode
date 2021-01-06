module Y2020.Prob23a ( sol1, sol2 ) where

import Control.Monad ( zipWithM_, foldM_ )
import Control.Monad.ST ( runST )
import Data.Int ( Int32 )
import Data.Vector.Unboxed ( (!), (//) )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

simulate :: Int -> Int -> V.Vector Int32 -> V.Vector Int32
simulate num start snxt = runST $ do
  nxt <- V.thaw snxt
  foldM_ (flip . const $ step nxt) start [1 .. num]
  V.freeze nxt where
    size = V.length snxt
    step nxt cur = do
      v <- V.iterateNM 5 (fmap fromIntegral . MV.read nxt) cur
      let dest = until (`V.notElem` V.init v) ((`mod` size) . pred) cur
      nDest <- MV.read nxt dest
      v!4 <$ zipWithM_ (MV.write nxt) [cur, dest, v!3] [fromIntegral (v!4), fromIntegral (v!1), nDest]

sol1 :: [Char] -> [Char]
sol1 inp = let
  cups = pred . read . pure <$> inp  -- To begin with 0
  cupLink = V.replicate (length cups) 0 // zip cups (tail $ fromIntegral <$> cycle cups)
  resLink = simulate 100 (head cups) cupLink
  in concat $ show . succ <$> (tail . take (length cups) $ iterate ((resLink !) . fromIntegral) 0)

sol2 :: [Char] -> Int
sol2 inp = let
  len = 1000000; cups = pred . read . pure <$> inp
  cupLink = (V.generate len (fromIntegral . succ) //) $ fmap fromIntegral <$> (zip cups (tail cups)
    <> [(pred len, head cups), (last cups, length cups)])
  resLink = simulate 10000000 (head cups) cupLink
  in product $ fromIntegral . succ <$> (take 2 . tail $ iterate ((resLink !) . fromIntegral) 0)