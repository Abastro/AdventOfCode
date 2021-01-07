module Y2020.Prob21 ( sol1, sol2 ) where

import Control.Monad ( guard )
import Data.Function ( on )
import Data.List ( sortBy, intercalate )
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Text.Read ( Read(..), Lexeme(..), lexP, parens, (+++) )

data Food = Food { ingreds :: S.HashSet String, allergens :: [String] } deriving Show
instance Read Food where
  readPrec = do
    ingr <- readIdentList $ pure ()
    alg <- parens $ expectP (Ident "contains") >> readIdentList (expectP $ Punc ",")
    pure $ Food (S.fromList ingr) alg
    where expectP lexeme = do l <- lexP; guard $ l == lexeme
          readIdentList sep = do Ident x <- lexP; pure [x]
            +++ do Ident x <- lexP; sep; (x :) <$> readIdentList sep

-- Gives map of allergen -> ingredients
candidates :: [Food] -> M.HashMap String (S.HashSet String)
candidates foods = foldr1 (M.unionWith S.intersection) $ candidate <$> foods where
  candidate (Food ingr alg) = M.fromList $ (, ingr) <$> alg

sol1 :: [String] -> Int
sol1 inp = let
    foods = read <$> inp
    allIngr = S.unions $ ingreds <$> foods
    nonAll = allIngr `S.difference` foldr1 S.union (candidates foods)
  in sum $ length . S.intersection nonAll . ingreds <$> foods

sol2 :: [String] -> String
sol2 inp = let
    initCand = candidates $ read <$> inp
    rmDup map = let done = foldr1 S.union $ M.filter ((== 1) . S.size) map in
      (`M.map` map) $ \cand ->
        if S.size cand == 1 then cand else cand `S.difference` done
    danger = map snd $ sortBy (compare `on` fst) . M.toList
      $ M.map (head . S.toList) $ until (all $ (== 1) . S.size) rmDup initCand
  in intercalate "," danger
