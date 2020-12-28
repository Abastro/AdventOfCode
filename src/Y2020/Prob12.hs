module Y2020.Prob12 where

import Data.Foldable ( Foldable(..) )

data Dir = N | E | S | W deriving (Enum, Show)
data TDir = L | R deriving Show
data Action = Move Dir Int | Turn TDir Int | Front Int
data Pos = Pos { east :: Int, north :: Int }

addPos :: Pos -> Pos -> Pos
addPos (Pos e n) (Pos e' n') = Pos (e + e') (n + n')

multPos :: Int -> Pos -> Pos
multPos m (Pos e n) = Pos (m * e) (m * n)

move :: Int -> Dir -> Pos
move n N = Pos 0 n
move n E = Pos n 0
move n S = Pos 0 (-n)
move n W = Pos (-n) 0

turn :: Int -> TDir -> Dir -> Dir
turn n L dir = toEnum $ (fromEnum dir - n) `mod` 4
turn n R dir = toEnum $ (fromEnum dir + n) `mod` 4

turn' :: Dir -> Pos -> Pos
turn' N p = p
turn' E (Pos x y) = Pos y (-x)
turn' S (Pos x y) = Pos (-x) (-y)
turn' W (Pos x y) = Pos (-y) x

readAct :: String -> Action
readAct ('N':str) = Move N $ read str
readAct ('E':str) = Move E $ read str
readAct ('S':str) = Move S $ read str
readAct ('W':str) = Move W $ read str
readAct ('L':str) = Turn L $ read str `div` 90
readAct ('R':str) = Turn R $ read str `div` 90
readAct ('F':str) = Front $ read str

action :: Action -> (Dir, Pos) -> (Dir, Pos)
action (Move dir n) (curDir, curPos) = (curDir, move n dir `addPos` curPos)
action (Turn td n) (curDir, curPos) = (turn n td curDir, curPos)
action (Front n) (curDir, curPos) = (curDir, move n curDir `addPos` curPos)

action' :: Action -> (Pos, Pos) -> (Pos, Pos)
action' (Move dir n) (wayPos, curPos) = (move n dir `addPos` wayPos, curPos)
action' (Turn td n) (wayPos, curPos) = (turn' (turn n td N) wayPos, curPos)
action' (Front n) (wayPos, curPos) = (wayPos, multPos n wayPos `addPos` curPos)

sol1 :: [String] -> Int
sol1 inp = let dest = foldl' (flip $ action . readAct) (E, Pos 0 0) inp in
  abs (east $ snd dest) + abs (north $ snd dest)

sol2 :: [String] -> Int
sol2 inp = let dest = foldl' (flip $ action' . readAct) (Pos 10 1, Pos 0 0) inp in
  abs (east $ snd dest) + abs (north $ snd dest)

