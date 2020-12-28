module Y2020.Prob11 where

import qualified Data.IntMap as M

-- Sorry, I'm lazy
count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

arrayLike :: [a] -> M.IntMap a
arrayLike = M.fromAscList . zip [0..]

type Seat = M.IntMap (M.IntMap Char)

countSeat :: Char -> Seat -> Int
countSeat ch = sum . fmap (length . M.filter (== ch))

seatAt :: Int -> Int -> Seat -> Char
seatAt i j = M.findWithDefault ' ' j . M.findWithDefault M.empty i

headOrSp :: [Char] -> Char
headOrSp [] = ' '
headOrSp (x:_) = x

process :: Seat -> Seat
process seat = let
    adj i j = seatAt <$> [i-1, i, i+1] <*> [j-1, j, j+1] <*> [seat]
    next i j 'L' = if count '#' (adj i j) == 0 then '#' else 'L'
    next i j '#' = if count '#' (adj i j) >= 5 then 'L' else  '#'
    next _ _ '.' = '.'
  in M.mapWithKey (M.mapWithKey . next) seat

process' :: Seat -> Seat
process' seat = let
    maxI = fst . M.findMax $ seat
    maxJ = fst . M.findMax . snd . M.findMax $ seat
    less i = [(i-1), (i-2) .. 0]
    more m i = [(i+1) .. m]
    adj i j =
      fmap (headOrSp . dropWhile (== '.'))
      $ zipWith (\p q -> seatAt p q seat)
      <$> [less i, repeat i, more maxI i] <*> [less j, repeat j, more maxJ j]
    next i j 'L' = if count '#' (adj i j) <= 0 then '#' else 'L'
    next i j '#' = if count '#' (adj i j) >= 6 then 'L' else '#'
    next _ _ '.' = '.'
  in M.mapWithKey (M.mapWithKey . next) seat

-- TODO Just use until
sol :: (Seat -> Seat) -> [[Char]] -> Int
sol pro inp = let seatings = iterate pro $ arrayLike . map arrayLike $ inp in
  head $ dropWhile (< 0)
  $ zipWith (\s t -> if s == t then countSeat '#' s else -1) seatings (tail seatings)

sol1 :: [[Char]] -> Int
sol1 = sol process

sol2 :: [[Char]] -> Int
sol2 = sol process'

