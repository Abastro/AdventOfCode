module Y2020.Prob4 where

import Control.Monad ( guard )

import Data.Char ( isDigit )
import Data.List ( (\\) )
import Data.Maybe ( maybeToList )

import Text.Read ( Read(..), readMaybe, prec, lexP, lift, (+++), pfail )
import Text.Read.Lex ( Lexeme(..), numberToInteger, expect )

import Text.ParserCombinators.ReadP ( count, satisfy )

import Common ( deintercalate )

data Height = CM Int | IN Int
newtype HairColor = HairColor String
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
newtype PassID = PassID String

eyeCl :: [(String, EyeColor)]
eyeCl = [("amb", Amb), ("blu", Blu), ("brn", Brn), ("gry", Gry), ("grn", Grn), ("hzl", Hzl), ("oth", Oth)]

instance Read Height where
  readPrec = prec 0 $ do
    Number num <- lexP
    Just n <- pure $ fromInteger <$> numberToInteger num
    (do Ident "cm" <- lexP; pure $ CM n) +++ (do Ident "in" <- lexP; pure $ IN n)

instance Read HairColor where
  readPrec = prec 0 $ do
    lift . expect $ Symbol "#"
    fmap HairColor $ lift . count 6
      $ satisfy (\c -> isDigit c || (c >= 'a' && c <= 'f'))

instance Read EyeColor where
  readPrec = prec 0 $ do
    Ident clr <- lexP
    maybe pfail pure $ lookup clr eyeCl

instance Read PassID where
  readPrec = prec 0 $ do
    fmap PassID . lift $ count 9 $ satisfy isDigit

data Credential = Credential {
  byr :: Int, iyr :: Int, eyr :: Int
  , hgt :: Height, hcl :: HairColor, ecl :: EyeColor
  , pid :: PassID
}

readCred :: [String] -> [(String, String)]
readCred cr = do
  prop <- concat $ words <$> cr
  let [field, val] = deintercalate ':' prop
  [(field, val)]

sol1 :: [String] -> Int
sol1 inp = length $ do
  cred <- readCred <$> deintercalate [] inp
  guard $ null (["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    \\ map fst cred)

sol2 :: [String] -> Int
sol2 inp = length $ do
  cred <- readCred <$> deintercalate [] inp
  let readField i = lookup i cred >>= readMaybe
  credent <- maybeToList $ Credential
    <$> readField "byr" <*> readField "iyr" <*> readField "eyr"
    <*> readField "hgt" <*> readField "hcl" <*> readField "ecl"
    <*> readField "pid"
  guard $ byr credent >= 1920 && byr credent <= 2002
  guard $ iyr credent >= 2010 && iyr credent <= 2020
  guard $ eyr credent >= 2020 && eyr credent <= 2030
  case hgt credent of
    CM h -> guard $ h >= 150 && h <= 193
    IN h -> guard $ h >= 59 && h <= 76

