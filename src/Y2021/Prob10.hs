{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2021.Prob10 ( sol10F, sol10S ) where
import Data.Foldable
import Data.Either
import Data.List

data ChToken = Parens | Square | Curly | Angle deriving (Eq, Enum)
handleToken st c = case c of
  '(' -> add Parens st; ')' -> rm Parens st
  '[' -> add Square st; ']' -> rm Square st
  '{' -> add Curly st; '}' -> rm Curly st
  '<' -> add Angle st; '>' -> rm Angle st
  where
    add t st = Right $ t : st
    rm t (t' : st) | t == t' = Right st; rm t _ = Left t

sol10F :: [String] -> Int
sol10F l = sum . fmap score . lefts $ foldlM handleToken [] <$> l where
  score Parens = 3; score Square = 57; score Curly = 1197; score Angle = 25137

sol10S :: [String] -> Int
sol10S l = median . fmap score . rights $ foldlM handleToken [] <$> l where
  score st = foldl' (\s d -> succ (fromEnum d) + 5 * s) 0 st
  median sc = sort sc !! (length sc `div` 2)