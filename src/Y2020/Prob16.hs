module Y2020.Prob16 where

import Prelude hiding ( lex )

import Data.Maybe ( fromJust )
import Data.List ( isPrefixOf, sortBy, inits, tails, (\\), transpose )
import Data.Function ( on )

import Text.Read ( readP_to_Prec,  Read(..) )
import Text.Read.Lex ( Lexeme(..), lex, expect, numberToInteger )
import qualified Text.ParserCombinators.ReadP as RP

import Common ( deintercalate )

data Field = Field { name :: String, predicate :: Int -> Bool }

instance Eq Field where
  (==) = (==) `on` name

instance Read Field where
  readPrec = readP_to_Prec $ const $ do
    field <- RP.munch (/= ':'); RP.char ':'
    preds <- RP.sepBy parseRange (expect $ Ident "or")
    pure $ Field field $ \p -> any ($ p) preds
    where
      numToInt = fromInteger . fromJust . numberToInteger
      parseRange = do
        Number n <- lex; Symbol "-" <- lex; Number m <- lex
        pure $ \p -> p >= numToInt n && p <= numToInt m

interpret :: [String] -> ([Field], [Int], [[Int]])
interpret inp = let
    [fields, ["your ticket:", your], "nearby tickets:" : nearby] = deintercalate [] inp
  in (read <$> fields, read <$> deintercalate ',' your,
    map read . deintercalate ',' <$> nearby)

sol1 :: [String] -> Int
sol1 inp = let
    (fields, _, nearby) = interpret inp
  in sum $ sum . filter (\i -> not $ any (($ i) . predicate) fields) <$> nearby

sol2 :: [String] -> Int
sol2 inp = let
    (fields, your, nearby) = interpret inp
    valid = filter (all $ \i -> any (($ i) . predicate) fields) nearby
    fieldFor vals = filter (\f -> all (predicate f) vals) fields
    posField = fieldFor <$> transpose valid -- Grouped by ticket -> Grouped by Fields
    corrField = map snd $ sortBy (compare `on` fst)
      $ map (\(xs, x) -> (fst x, head $ foldr (flip (\\)) (snd x) xs))
      $ (zip <$> inits . map snd <*> head . tails)
      $ sortBy (compare `on` length . snd) $ zip [0..] posField
  in product $ map snd . filter (isPrefixOf "departure" . name . fst)
    $ zip corrField your

