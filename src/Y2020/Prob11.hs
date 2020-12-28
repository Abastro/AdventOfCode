module Y2020.Prob11 where

import Control.Monad ( (>=>) )

import Data.List ( find )
import Data.Maybe ( catMaybes, mapMaybe )
import Data.Function ( (&) )
import qualified Data.Vector as V

import Common ( count )

type Seat = V.Vector (V.Vector Char)

countSeat :: Char -> Seat -> Int
countSeat ch = count ch . (V.toList >=> V.toList)

seatAt :: Int -> Int -> Seat -> Maybe Char
seatAt i j = (V.!? i) >=> (V.!? j)

process :: Seat -> Seat
process seat = let
    adj i j = catMaybes $ seatAt <$> [i-1, i, i+1] <*> [j-1, j, j+1] <*> [seat]
    next i j 'L' = if count '#' (adj i j) == 0 then '#' else 'L'
    next i j '#' = if count '#' (adj i j) >= 5 then 'L' else  '#'
    next _ _ '.' = '.'
  in V.imap (V.imap . next) seat

process' :: Seat -> Seat
process' seat = let
    maxI = V.length seat - 1
    maxJ = V.length (seat V.! 0) - 1
    less i = [(i-1), (i-2) .. 0]
    more m i = [(i+1) .. m]
    adj i j =
      catMaybes $ mapMaybe (find (/= Just '.'))
      $ zipWith (\p q -> seatAt p q seat)
      <$> [less i, repeat i, more maxI i] <*> [less j, repeat j, more maxJ j]
    next i j 'L' = if count '#' (adj i j) == 0 then '#' else 'L'
    next i j '#' = if count '#' (adj i j) >= 6 then 'L' else '#'
    next _ _ '.' = '.'
  in V.imap (V.imap . next) seat

-- TODO: Optimize, how?
sol :: (Seat -> Seat) -> [[Char]] -> Int
sol pro inp = let seats = V.fromList . map V.fromList $ inp
  in (seats, pro seats)
  & until (uncurry (==)) (\(_, next) -> (next, pro next))
  & countSeat '#' . snd

sol1 :: [[Char]] -> Int
sol1 = sol process

sol2 :: [[Char]] -> Int
sol2 = sol process'

