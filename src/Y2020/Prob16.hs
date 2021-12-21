module Y2020.Prob16 ( solP16F, solP16S ) where

import Data.List
import Data.Function ( on, (&) )
import Text.Read
import Text.ParserCombinators.ReadP
import Common

data Field = Field { name :: String, predicate :: Int -> Bool }
instance Eq Field where
  (==) = (==) `on` name
instance Read Field where
  readPrec = prec 0 $ do
    field <- lift $ munch (/= ':'); lift $ string ":"
    preds <- liftFn (`sepBy` (skipSpaces >> string "or")) $ do
      n <- readPrec; Symbol "-" <- lexP; m <- readPrec
      pure $ \p -> p >= n && p <= m
    pure $ Field field $ \p -> any ($ p) preds

interpret :: [String] -> ([Field], [Int], [[Int]])
interpret inp = let
  [fields, ["your ticket:", your], "nearby tickets:" : nearby] = deintercalate [] inp
  in (read <$> fields, read <$> deintercalate ',' your,
    map read . deintercalate ',' <$> nearby)

solP16F :: [String] -> Int
solP16F inp = let (fields, _, nearby) = interpret inp in
  sum $ sum . filter (\i -> not $ any (($ i) . predicate) fields) <$> nearby

solP16S :: [String] -> Int
solP16S inp = let
  (fields, your, nearby) = interpret inp
  valid = filter (all $ \i -> any (($ i) . predicate) fields) nearby
  fieldFor vals = filter (\f -> all (predicate f) vals) fields
  posField = fieldFor <$> transpose valid -- Grouped by ticket -> Grouped by Fields
  corrField = zip [0..] posField
    & sortBy (compare `on` length . snd) -- Exploits current circumstance
    & (zip <$> inits . map snd <*> head . tails)
    & map (\(xs, x) -> (fst x, head $ foldr (flip (\\)) (snd x) xs))
    & map snd . sortBy (compare `on` fst)
  in zip corrField your
  & filter (isPrefixOf "departure" . name . fst) & product . map snd