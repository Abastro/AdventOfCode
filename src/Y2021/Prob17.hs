module Y2021.Prob17 ( sol17F, sol17S ) where
import Data.Char
import Data.Ratio
import qualified Data.Set as S

readArea s = (xMin, xMax, yMin, yMax) where
  replChar c = if isDigit c || c == '-' then c else ' '
  [xMin, xMax, yMin, yMax] = read @Int <$> words (replChar <$> s)

sol17F :: String -> Int
sol17F s = ((-yMin) * (-yMin - 1)) `div` 2 where (_, _, yMin, _) = readArea s -- NOTE: yMin < 0

sol17S :: String -> Int
sol17S s = S.size . S.fromList $ concatMap poss [1 .. -2 * yMin] where
  (xMin, xMax, yMin, yMax) = readArea s
  stopXRange = dropWhile (\x -> x * succ x < 2 * xMin) $ takeWhile (\x -> x * succ x <= 2 * xMax) [0..]
  stopXMin = head stopXRange; stopXMax = last stopXRange
  simvOf :: Int -> Int -> Ratio Int
  simvOf n y = let n' = fromIntegral n; y' = fromIntegral y in y' / n' + (n' - 1) / 2
  vyMin n = ceiling $ simvOf n yMin; vyMax n = floor $ simvOf n yMax
  vxMin n = ceiling $ simvOf n xMin; vxMax n = floor $ simvOf n xMax
  vxMin' n = let v = vxMin n in if v < n then stopXMin else v
  vxMax' n = let v = vxMax n in if v < n then stopXMax else v
  poss n = [(x, y) | x <- [vxMin' n .. vxMax' n], y <- [vyMin n .. vyMax n]]