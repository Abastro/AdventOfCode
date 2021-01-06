module Y2020.Prob7 ( sol1, sol2 ) where

import Control.Monad ( guard )
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as M
import Text.Read ( prec, readPrec, lexP, lift, (+++) )
import Text.ParserCombinators.ReadP ( string, skipSpaces, sepBy1 )
import Text.Read.Lex ( Lexeme(..), expect )
import GHC.Generics ( Generic )
import Common ( liftFn )

data Bag = Bag String String deriving (Eq, Ord, Generic, Show)
instance Hashable Bag
instance Read Bag where
  readPrec = prec 0 $ do
    Ident tone <- lexP; Ident color <- lexP
    Ident plur <- lexP; guard $ plur == "bag" || plur == "bags"
    pure $ Bag tone color

type BagMap = M.HashMap Bag
newtype Rule = Rule { getRule :: (Bag, BagMap Int) }
instance Read Rule where
  readPrec = prec 0 $ do
    mainBag <- readPrec; lift . expect $ Ident "contain"
    bags <- readConts; lift . expect $ Symbol "."
    pure $ Rule . (mainBag, ) . M.fromList $ bags
    where
      readConts = lift (skipSpaces >> string "no other bags" >> pure [])
        +++ liftFn (`sepBy1` string ",") (do n <- readPrec; (, n) <$> readPrec)

-- Laziness works like cache here
unpacked :: BagMap (BagMap Int) -> BagMap (BagMap Int)
unpacked bags = let
  unpacked = M.mapWithKey (\bag num -> (* num) <$> result M.! bag)
  result = (`M.mapWithKey` bags) $ \this content ->
    foldr (M.unionWith (+)) (M.singleton this 1) $ unpacked content
  in result

sol1 :: [String] -> Int
sol1 inp = let rules = M.fromList $ getRule . read <$> inp in
  subtract 1 $ (M.! Bag "shiny" "gold")
  $ foldr (M.unionWith (+)) M.empty
  $ M.map (const 1) . (unpacked rules M.!) <$> M.keys rules

sol2 :: [String] -> Int
sol2 inp = let rules = M.fromList $ getRule . read <$> inp in
  subtract 1 $ sum $ unpacked rules M.! Bag "shiny" "gold"