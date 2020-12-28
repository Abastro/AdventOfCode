module Y2020.Prob7 where

import Prelude hiding ( lex )

import Control.Monad ( guard )

import Data.Maybe ( fromJust )
import Data.Monoid ( Sum(..) )
import Data.Foldable ( Foldable(..) )
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as M

import Text.ParserCombinators.ReadP
import Text.Read.Lex ( Lexeme(..), lex, numberToInteger )

import GHC.Generics ( Generic )

data Bag = Bag String String deriving (Eq, Ord, Generic, Show)
instance Hashable Bag

type BagMap = M.HashMap Bag

readBag :: ReadP Bag
readBag = do
  Ident tone <- lex; Ident color <- lex
  Ident plur <- lex; guard $ plur == "bag" || plur == "bags"
  pure $ Bag tone color

readRule :: ReadP (Bag, BagMap Int)
readRule = do
  mainBag <- readBag; Ident "contain" <- lex
  bags <- readConts
  char '.' >> pure (mainBag, M.fromList bags)
  where
    readConts = (skipSpaces >> string "no other bags" >> pure []) <++
      sepBy1 (do
        Number n <- lex
        Just num <- pure $ fromInteger <$> numberToInteger n
        (, num) <$> readBag) (char ',')

-- TODO More efficient implementation
unpack :: BagMap (BagMap Int) -> Bag -> BagMap Int
unpack bags this = fold $ do
  content <- M.lookup this bags
  let unpacked = M.mapWithKey (\bag num -> (* num) <$> unpack bags bag) content
  pure $ foldr (M.unionWith (+)) (M.singleton this 1) unpacked

readRules :: [String] -> BagMap (BagMap Int)
readRules inp = M.fromList $ do
  (rule, "") <- inp >>= readP_to_S readRule
  pure rule

sol1 :: [String] -> Int
sol1 inp = let rules = readRules inp in
  subtract 1 $ fromJust $ M.lookup (Bag "shiny" "gold")
  $ foldr (M.unionWith (+)) M.empty
  $ M.map (const 1) . unpack rules <$> M.keys rules

sol2 :: [String] -> Int
sol2 inp = let rules = readRules inp in
  subtract 1 $ getSum $ foldMap Sum $ unpack rules (Bag "shiny" "gold")

