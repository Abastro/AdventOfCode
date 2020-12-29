module Y2020.Prob16 ( sol1, sol2 ) where

import Data.List ( isPrefixOf, sortBy, inits, tails, (\\), transpose )
import Data.Function ( on, (&) )
import Text.Read ( Read(..), lift, prec, lexP )
import Text.Read.Lex ( Lexeme(..), expect )
import Text.ParserCombinators.ReadP ( munch, char, sepBy )
import Common ( deintercalate, numToInt, liftFn )

data Field = Field { name :: String, predicate :: Int -> Bool }
instance Eq Field where
  (==) = (==) `on` name

instance Read Field where
  readPrec = prec 0 $ do
    field <- lift $ munch (/= ':'); lift $ char ':'
    preds <- liftFn (`sepBy` expect (Ident "or")) $ do
      Number n <- lexP; Symbol "-" <- lexP; Number m <- lexP
      pure $ \p -> p >= numToInt n && p <= numToInt m
    pure $ Field field $ \p -> any ($ p) preds

interpret :: [String] -> ([Field], [Int], [[Int]])
interpret inp = let
    [fields, ["your ticket:", your], "nearby tickets:" : nearby] = deintercalate [] inp
  in (read <$> fields, read <$> deintercalate ',' your,
    map read . deintercalate ',' <$> nearby)

sol1 :: [String] -> Int
sol1 inp = let (fields, _, nearby) = interpret inp in
  sum $ sum . filter (\i -> not $ any (($ i) . predicate) fields) <$> nearby

sol2 :: [String] -> Int
sol2 inp = let
    (fields, your, nearby) = interpret inp
    valid = filter (all $ \i -> any (($ i) . predicate) fields) nearby
    fieldFor vals = filter (\f -> all (predicate f) vals) fields
    posField = fieldFor <$> transpose valid -- Grouped by ticket -> Grouped by Fields
    corrField = zip [0..] posField
      & sortBy (compare `on` length . snd)
      & (zip <$> inits . map snd <*> head . tails)
      & map (\(xs, x) -> (fst x, head $ foldr (flip (\\)) (snd x) xs))
      & map snd . sortBy (compare `on` fst)
  in zip corrField your
  & filter (isPrefixOf "departure" . name . fst)
  & product . map snd
