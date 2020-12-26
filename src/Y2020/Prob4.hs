module Y2020.Prob4 where

import Prelude hiding ( lex )

import Control.Monad ( (>=>), guard )

import Data.Char ( isLetter, isSpace, isDigit )
import Data.List ( (\\) )
import Data.Maybe ( maybeToList )

import Text.Read ( Read(..), readMaybe, readP_to_Prec )
import Text.Read.Lex ( Lexeme(..), lex, numberToInteger )

import Text.ParserCombinators.ReadP
    ( ReadP,
      char,
      string,
      (<++),
      (+++),
      satisfy,
      pfail,
      many,
      count,
      munch,
      readP_to_S,
      sepBy,
      skipMany,
      skipSpaces )

data Height = CM Int | IN Int
newtype HairColor = HairColor String
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
newtype PassID = PassID String

eyeCl :: [(String, EyeColor)]
eyeCl = [("amb", Amb), ("blu", Blu), ("brn", Brn), ("gry", Gry), ("grn", Grn), ("hzl", Hzl), ("oth", Oth)]

instance Read Height where
  readPrec = readP_to_Prec $ const $ do
    Number num <- lex
    Just n <- pure $ fromInteger <$> numberToInteger num
    (do Ident "cm" <- lex; pure $ CM n) +++ (do Ident "in" <- lex; pure $ IN n)

instance Read HairColor where
  readPrec = readP_to_Prec $ const $ do
    char '#'
    fmap HairColor $ count 6
      $ satisfy (\c -> isDigit c || (c >= 'a' && c <= 'f'))

instance Read EyeColor where
  readPrec = readP_to_Prec $ const $ do
    Ident clr <- lex
    maybe pfail pure $ lookup clr eyeCl

instance Read PassID where
  readPrec = readP_to_Prec $ const $
    fmap PassID $ count 9 $ satisfy isDigit

data Credential = Credential {
  byr :: Int, iyr :: Int, eyr :: Int
  , hgt :: Height, hcl :: HairColor, ecl :: EyeColor
  , pid :: PassID
}

readCred :: ReadP [[(String, String)]]
readCred = (<* skipSpaces) . (`sepBy` string "\n") . many $ do
  field <- munch isLetter; char ':'
  name <- munch $ not . isSpace
  skipMany $ char ' '
  (char '\n' >> pure ()) <++ pure ()
  pure (field, name)

sol1 :: String -> Int
sol1 inp = length $ do
  (creds, "") <- readP_to_S readCred inp
  credential <- creds
  guard $ null (["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    \\ map fst credential)

sol2 :: String -> Int
sol2 inp = length $ do
  (creds, "") <- readP_to_S readCred inp
  cred <- creds
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

