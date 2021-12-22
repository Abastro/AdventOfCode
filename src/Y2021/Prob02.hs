{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2021.Prob02 ( sol2F, sol2S ) where
import Data.Foldable
import Data.Monoid

sol2F :: [String] -> Int   
sol2F l = x * y where
  (Sum x, Sum y) = foldMap (\[a, n] -> act a $ Sum $ read n) $ words <$> l
  act "forward" n = (n, 0)
  act "down" n = (0, n)
  act "up" n = (0, -n)

sol2S :: [String] -> Int
sol2S l = x * y where
  (x, y, _) = foldl' (flip $ \[a, n] -> act a (read n)) (0, 0, 0) (words <$> l)
  act "forward" n (x, y, aim) = (x + n, y + n * aim, aim)
  act "down" n (x, y, aim) = (x, y, aim + n)
  act "up" n (x, y, aim) = (x, y, aim - n)