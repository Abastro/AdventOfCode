module Y2020.Prob7 ( solP7F, solP7S ) where

import Control.Monad ( guard )
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as M
import Text.Read ( Read(..), Lexeme(..), prec, lexP, lift, (+++) )
import Text.ParserCombinators.ReadP ( string, skipSpaces, sepBy1 )
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
    mainBag <- readPrec; Ident "contain" <- lexP
    bags <- readConts; Symbol "." <- lexP
    pure $ Rule . (mainBag, ) . M.fromList $ bags where
      readConts = lift (skipSpaces >> string "no other bags" >> pure [])
        +++ liftFn (`sepBy1` string ",") (flip (,) <$> readPrec <*> readPrec)

-- Laziness works like cache here
unpacked :: BagMap (BagMap Int) -> BagMap (BagMap Int)
unpacked bags = result where
  unpacked = M.mapWithKey (\bag num -> (* num) <$> result M.! bag)
  result = (`M.mapWithKey` bags) $ \this content ->
    foldr (M.unionWith (+)) (M.singleton this 1) $ unpacked content

solP7F :: [String] -> Int
solP7F inp = let rules = M.fromList $ getRule . read <$> inp in
  subtract 1 $ (M.! Bag "shiny" "gold")
  $ foldr (M.unionWith (+)) M.empty
  $ M.map (const 1) . (unpacked rules M.!) <$> M.keys rules

solP7S :: [String] -> Int
solP7S inp = let rules = M.fromList $ getRule . read <$> inp in
  subtract 1 $ sum $ unpacked rules M.! Bag "shiny" "gold"