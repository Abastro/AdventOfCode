-- |Fits entire row within Word256
module Y2021.Prob25 ( sol25F ) where
import Data.Bits
import Data.WideWord.Word256
import qualified Data.Vector.Primitive as PV
type DMap = PV.Vector Word256
fromBits = sum . zipWith (\x f -> if f then x else 0) (map bit [0..])

moveCs :: Int -> (DMap, DMap) -> (DMap, DMap)
moveCs width (rs, ds) = (rs', ds') where
  rs' = moveWith rightSh leftSh rs ds
  ds' = moveWith downSh upSh ds rs'
  moveWith sh unsh toMove aid = PV.zipWith (.|.) (unsh blocker) $ PV.zipWith xor shifted blocker where
    shifted = sh toMove
    blocker = PV.zipWith (.&.) shifted $ PV.zipWith (.|.) toMove aid

  totalMask = pred $ 1 `shiftL` width
  leftSh m = PV.map (\i -> totalMask .&. (i `shiftL` pred width) .|. (i `shiftR` 1)) m
  rightSh m = PV.map (\i -> (i `shiftR` pred width) .|. (i `shiftL` 1) .&. totalMask) m
  upSh m = PV.snoc (PV.tail m) (PV.head m)
  downSh m = PV.cons (PV.last m) (PV.init m)

sol25F :: [String] -> Int
sol25F l = length $ ites (PV.fromList rs, PV.fromList ds) where
  w = length $ head l
  rs = fromBits . map (== '>') <$> l; ds = fromBits . map (== 'v') <$> l
  ites rds = let rds' = moveCs w rds in rds : if rds == rds' then [] else ites rds'