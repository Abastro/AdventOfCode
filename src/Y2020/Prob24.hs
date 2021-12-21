{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2020.Prob24 ( solP24F, solP24S ) where

import Data.Hashable ( Hashable )
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import GHC.Generics ( Generic )
import Common ( count, applyN )

data Pos = Pos { px :: !Int, py :: !Int} deriving (Eq, Generic)
instance Hashable Pos
instance Semigroup Pos where Pos x y <> Pos x' y' = Pos (x + x') (y + y')
instance Monoid Pos where mempty = Pos 0 0

moveList :: [Char] -> [Pos]
moveList [] = []
moveList ('e':xs)     = Pos 1 0 : moveList xs
moveList ('s':'e':xs) = Pos 0 (-1) : moveList xs
moveList ('s':'w':xs) = Pos (-1) (-1) : moveList xs
moveList ('w':xs)     = Pos (-1) 0 : moveList xs
moveList ('n':'w':xs) = Pos 0 1 : moveList xs
moveList ('n':'e':xs) = Pos 1 1 : moveList xs

blackTiles :: [String] -> S.HashSet Pos
blackTiles inp = S.fromMap . M.map (const ())
  $ M.filter ((== 1) . (`mod` 2))
  $ M.fromListWith (+) $ (`zip` repeat 1) $ mconcat . moveList <$> inp

solP24F :: [String] -> Int
solP24F = S.size . blackTiles

solP24S :: [String] -> Int
solP24S inp = S.size $ applyN 100 stepTiles $ blackTiles inp where
  nbs = moveList $ "e"<>"se"<>"sw"<>"w"<>"nw"<>"ne"
  nMax f set = succ $ S.foldl' (flip $ max . f) minBound set
  nMin f set = pred $ S.foldl' (flip $ min . f) maxBound set
  stepTiles tiles = S.fromList $ filter next
    $ Pos <$> [nMin px tiles .. nMax px tiles] <*> [nMin py tiles .. nMax py tiles] where
    next pos
      | pos `S.member` tiles = (||) <$> (== 1) <*> (== 2) $ numNbs pos
      | otherwise = (== 2) $ numNbs pos
    numNbs pos = count True $ (`S.member` tiles) . (pos <>) <$> nbs