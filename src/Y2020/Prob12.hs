module Y2020.Prob12 ( sol1, sol2 ) where

import Data.Foldable ( Foldable(..) )

data Dir = N | E | S | W deriving (Enum, Show)
data TDir = L | R deriving (Eq, Show)
data Action = Move Dir Int | Turn TDir Int | Front Int
data Pos = Pos { east :: !Int, north :: !Int }
addPos :: Pos -> Pos -> Pos
addPos (Pos e n) (Pos e' n') = Pos (e + e') (n + n')
multPos :: Int -> Pos -> Pos
multPos m (Pos e n) = Pos (m * e) (m * n)
move :: Int -> Dir -> Pos
move n N = Pos 0 n; move n S = Pos 0 (-n)
move n E = Pos n 0; move n W = Pos (-n) 0

turn :: Int -> TDir -> Dir -> Dir
turn n tdir dir = toEnum $ (fromEnum dir + if tdir == L then -n else n) `mod` 4
rotate :: Dir -> Pos -> Pos
rotate N p = p;                  rotate S (Pos x y) = Pos (-x) (-y)
rotate E (Pos x y) = Pos y (-x); rotate W (Pos x y) = Pos (-y) x

readAct :: String -> Action
readAct (c:str) = act c $ read str where
  act 'N' = Move N; act 'E' = Move E; act 'S' = Move S; act 'W' = Move W
  act 'L' = Turn L . (`div` 90); act 'R' = Turn R . (`div` 90); act 'F' = Front

sol1 :: [String] -> Int
sol1 inp = abs (east $ snd dest) + abs (north $ snd dest) where
  dest = foldl' (flip $ action . readAct) (E, Pos 0 0) inp
  action (Move dir n) (curDir, curPos) = (curDir, move n dir `addPos` curPos)
  action (Turn td n) (curDir, curPos) = (turn n td curDir, curPos)
  action (Front n) (curDir, curPos) = (curDir, move n curDir `addPos` curPos)

sol2 :: [String] -> Int
sol2 inp = abs (east $ snd dest) + abs (north $ snd dest) where
  dest = foldl' (flip $ action . readAct) (Pos 10 1, Pos 0 0) inp
  action (Move dir n) (wayPos, curPos) = (move n dir `addPos` wayPos, curPos)
  action (Turn td n) (wayPos, curPos) = (rotate (turn n td N) wayPos, curPos)
  action (Front n) (wayPos, curPos) = (wayPos, multPos n wayPos `addPos` curPos)