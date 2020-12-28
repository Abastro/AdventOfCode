module Y2020.Prob2 where

import Control.Monad ( guard )

import Text.Read ( Read(..), lexP, lift, prec )
import Text.Read.Lex ( Lexeme(..), expect )

import Common ( numToInt )

data PassScheme = PassScheme {
  lowBnd :: Int, highBnd :: Int
  , key :: Char, password :: String
}

instance Read PassScheme where
  readPrec = prec 0 $ do
    Number low <- lexP; lift . expect $ Symbol "-"; Number high <- lexP
    Ident k <- lexP; lift . expect $ Symbol ":"; Ident pw <- lexP
    pure PassScheme {
      lowBnd = numToInt low, highBnd = numToInt high
      , key = head k, password = pw
    }

sol1 :: [String] -> Int
sol1 list = length $ do
  (scheme, "") <- list >>= reads
  let numKey = length $ filter (== key scheme) $ password scheme
  guard (numKey >= lowBnd scheme && numKey <= highBnd scheme)

sol2 :: [String] -> Int
sol2 list = length $ do
  (scheme, "") <- list >>= reads
  let locs = [password scheme !! (lowBnd scheme-1), password scheme !! (highBnd scheme-1)]
  guard $ [key scheme] == filter (== key scheme) locs
