module Common (
  boolToMaybe, count, deintercalate,
  numToInt, liftFn
) where

import Data.List (groupBy)
import Data.Maybe (fromJust)
import Data.Function (on)
import Text.Read.Lex ( Number, numberToInteger )
import Text.ParserCombinators.ReadP ( ReadP )
import Text.ParserCombinators.ReadPrec ( ReadPrec, readP_to_Prec, readPrec_to_P )

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

deintercalate :: Eq a => a -> [a] -> [[a]]
deintercalate p = filter (/= [p]) . groupBy ((==) `on` (== p))

numToInt :: Num a => Number -> a
numToInt = fromInteger . fromJust . numberToInteger

liftFn :: (ReadP a -> ReadP b) -> (ReadPrec a -> ReadPrec b)
liftFn f = readP_to_Prec . (f .) . readPrec_to_P