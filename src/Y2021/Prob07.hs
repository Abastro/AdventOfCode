module Y2021.Prob07 where
import Common
import Data.List

sol7F :: String -> Int -- Maybe comma format?
sol7F s = sum $ abs . (median -) <$> crabs where
  crabs = sort $ read @Int <$> deintercalate ',' s
  median = crabs !! (length crabs `div` 2)

sol7S :: String -> Int
sol7S s = minimum $ fuelOf <$> [pred mean .. succ mean] where
  crabs = sort $ read @Int <$> deintercalate ',' s
  mean = sum crabs `div` length crabs
  fuelOf k = sum $ (\n -> n * (n + 1) `div` 2) . abs . (k -) <$> crabs