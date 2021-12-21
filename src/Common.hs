module Common (
  c2word, inRange, applyN, boolToMaybe, count, deintercalate, liftFn
) where

import Data.Char (ord)
import Data.Word (Word8)
import Data.List (groupBy)
import Data.Function (on)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec

-- Unsafe stuff
c2word :: Char -> Word8
c2word = fromIntegral . ord

inRange :: (Int, Int) -> Int -> Bool
inRange (lb, ub) = (&&) <$> (>= lb) <*> (< ub)

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = go n x where
  go 0 x = x; go n x = let x' = f x in x' `seq` go (pred n) x'

boolToMaybe :: Bool -> Maybe ()
boolToMaybe f = if f then Just () else Nothing

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

deintercalate :: Eq a => a -> [a] -> [[a]]
deintercalate p = filter (/= [p]) . groupBy ((==) `on` (== p))

liftFn :: (ReadP a -> ReadP b) -> (ReadPrec a -> ReadPrec b)
liftFn f = readP_to_Prec . (f .) . readPrec_to_P