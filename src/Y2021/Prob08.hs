module Y2021.Prob08 ( sol8F, sol8S ) where
import Common
import Text.Read
import Data.Bits
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as UV

data Display = Display { uniqs :: UV.Vector Int, outp :: UV.Vector Int }
instance Read Display where
  readPrec = Display <$> readBitsets 10 <*> (do Punc "|" <- lexP; readBitsets 4) where
    readBitsets n = UV.replicateM n $ do Ident x <- lexP; pure $ bitset x
    bitset s = foldl' (.|.) 0 $ bit . subtract (ord 'a') . ord <$> s

sol8F :: [String] -> Int
sol8F l = count 2 outs + count 4 outs + count 3 outs + count 7 outs where -- 1, 4, 7, 8
  outs = [popCount w | Display _ o <- read <$> l, w <- UV.toList o]

sol8S :: [String] -> Int -- TODO: Cleanup (Or better solution)
sol8S l = sum $ go . read <$> l where
  go disp = num $ (ws IM.!) <$> UV.toList (outp disp) where
    num = foldl' (\n d -> n * 10 + d) 0
    m `subset` n = if (m .&. complement n) == 0 then 1 else 0
    cns n = filter ((== n) . popCount) (UV.toList $ uniqs disp); cn = head . cns
    w1 = cn 2; w4 = cn 4; w7 = cn 3; w8 = cn 7
    [w6, w0, w9] = sortOn (\n -> w4 `subset` n + w7 `subset` n) (cns 6)
    [w2, w3, w5] = sortOn (\n -> n `subset` w6 + n `subset` w9) (cns 5)
    ws = IM.fromList $ zip [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9] [0..]