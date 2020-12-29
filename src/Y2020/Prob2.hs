module Y2020.Prob2 ( sol1, sol2 ) where

import Control.Monad ( guard )
import Data.Function ( on )
import Text.Read ( Read(..), lexP, lift, prec )
import Text.Read.Lex ( Lexeme(..), expect )
import Common ( numToInt, count )

data PassScheme = PassScheme Int Int Char String
instance Read PassScheme where
  readPrec = prec 0 $ do
    Number low <- lexP; lift . expect $ Symbol "-"; Number high <- lexP
    Ident k <- lexP; lift . expect $ Symbol ":"; Ident pw <- lexP
    pure $ PassScheme (numToInt low) (numToInt high) (head k) pw

sol1 :: [String] -> Int
sol1 list = length $ do
  PassScheme low high key pw <- read <$> list
  guard (count key pw >= low && count key pw <= high)

sol2 :: [String] -> Int
sol2 list = length $ do
  PassScheme low high key pw <- read <$> list
  guard $ ((/=) `on` (== key)) (pw !! (low-1)) (pw !! (high-1))
