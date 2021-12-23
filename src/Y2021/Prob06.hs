module Y2021.Prob06 ( sol6 ) where
import Common
import qualified Data.Vector.Unboxed as UV

next :: UV.Vector Int -> UV.Vector Int
next v = UV.accum (+) (UV.tail v `UV.snoc` UV.head v) [(6, v UV.! 0)]

sol6 :: (String, Int) -> Int
sol6 (s, day) = UV.sum $ applyN day next b where
  fish = read @Int <$> deintercalate ',' s
  b = UV.generate 9 (`count` fish)