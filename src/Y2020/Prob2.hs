module Y2020.Prob2 ( solP2F, solP2S ) where

import Control.Monad ( guard )
import Data.Function ( on )
import Text.Read
import Common

data PassScheme = PassScheme Int Int Char String
instance Read PassScheme where
  readPrec = prec 0 $ do
    low <- readPrec; Symbol "-" <- lexP; high <- readPrec
    Ident k <- lexP; Symbol ":" <- lexP; Ident pw <- lexP
    pure $ PassScheme low high (head k) pw

solP2F :: [String] -> Int
solP2F list = length $ do PassScheme low high key pw <- read <$> list
                          guard (count key pw >= low && count key pw <= high)

solP2S :: [String] -> Int
solP2S list = length $ do PassScheme low high key pw <- read <$> list
                          guard $ ((/=) `on` (== key)) (pw !! (low-1)) (pw !! (high-1))