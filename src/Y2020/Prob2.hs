module Y2020.Prob2 where

import Prelude hiding ( lex )

import Control.Monad ( guard )

import Data.Maybe ( fromJust )

import Text.ParserCombinators.ReadP ( ReadP, readP_to_S )
import Text.Read.Lex
    ( Lexeme(..), expect, lex, numberToInteger )

data PassScheme = PassScheme {
  lowBnd :: Int
  , highBnd :: Int
  , key :: Char
  , password :: String
}

parseScheme :: ReadP PassScheme
parseScheme = do
  Number low <- lex; expect $ Symbol "-"; Number high <- lex
  Ident k <- lex; expect $ Symbol ":"; Ident pw <- lex
  pure PassScheme {
    lowBnd = numToInt low
    , highBnd = numToInt high
    , key = head k
    , password = pw
  } where numToInt = fromInteger . fromJust . numberToInteger

sol1 :: [String] -> Int
sol1 list = length $ do
  (scheme, "") <- list >>= readP_to_S parseScheme
  let numKey = length $ filter (== key scheme) $ password scheme
  guard (numKey >= lowBnd scheme && numKey <= highBnd scheme)

sol2 :: [String] -> Int
sol2 list = length $ do
  (scheme, "") <- list >>= readP_to_S parseScheme
  let locs = [password scheme !! (lowBnd scheme-1), password scheme !! (highBnd scheme-1)]
  guard $ [key scheme] == filter (== key scheme) locs
