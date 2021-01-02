module Y2020.Prob24 ( sol1, sol2 ) where

import Data.List ( foldl' )
import Data.Hashable ( Hashable )
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import GHC.Generics ( Generic )
import Common ( count )

data Pos = Pos Int Int deriving (Eq, Generic)
instance Hashable Pos
instance Semigroup Pos where
  Pos x y <> Pos x' y' = Pos (x + x') (y + y')
instance Monoid Pos where
  mempty = Pos 0 0

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

stepTiles :: S.HashSet Pos -> S.HashSet Pos
stepTiles tiles = S.filter next $ S.unions $ (\m -> S.map (<> m) tiles) <$> nbs where
  next pos
    | pos `S.member` tiles = (`elem` [1,2]) $ numNbs pos
    | otherwise = (== 2) $ numNbs pos
  numNbs pos = count True $ (`S.member` tiles) . (pos <>) <$> nbs
  nbs = moveList $ "e"<>"se"<>"sw"<>"w"<>"nw"<>"ne"

sol1 :: [String] -> Int
sol1 = S.size . blackTiles

-- TODO: Optimize sol2
sol2 :: [String] -> Int
sol2 inp = let
  tiles = blackTiles inp
  in S.size $ foldl' (flip $ const stepTiles) tiles [1..100]