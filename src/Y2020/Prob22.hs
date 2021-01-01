module Y2020.Prob22 ( sol1, sol2 ) where

import qualified Data.HashSet as S
import Common ( deintercalate )

procWin :: Bool -> ([a], [a]) -> ([a], [a])
procWin fstWin (c : cs, c' : cs') = if fstWin
  then (cs <> [c, c'], cs') else (cs, cs' <> [c', c])

sol1 :: [String] -> Int
sol1 inp = let
    combat (l, l') = procWin (head l > head l') (l, l')
    [x, y] = tail <$> deintercalate [] inp
    (res1, res2) = until (\(p, q) -> null p || null q) combat (read <$> x, read <$> y)
  in sum $ zipWith (*) [1..] $ reverse res1 <> reverse res2

-- Same cards same order -> Player 1 wins
sol2 :: [String] -> Int
sol2 inp = let
    reCombat _ (l, []) = (True, l)
    reCombat _ ([], l) = (False, l)
    reCombat s deck@(c : cs, c' : cs')
      | deck `S.member` s = (True, fst deck)
      | c <= length cs && c' <= length cs' = reCombat newS $ procWin win deck
      | otherwise = reCombat newS $ procWin (c > c') deck
      where
        newS = S.insert deck s
        win = fst $ reCombat S.empty (take c cs, take c' cs')
    [x, y] = tail <$> deintercalate [] inp
  in sum $ zipWith (*) [1..] $ reverse . snd $ reCombat S.empty (read <$> x, read <$> y)
