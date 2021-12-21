{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2020.Prob8 ( solP8F, solP8S ) where

import Data.Foldable ( Foldable(..) )
import qualified Data.Vector as V
import qualified Data.IntMap as M
import qualified Data.IntSet as S

data Instr = Acc Int | Jmp Int | Nop Int
readInstr :: [String] -> V.Vector Instr
readInstr strs = V.fromList $ do
  str <- strs; let num = read $ dropWhile (== '+') $ drop 4 str
  pure $ (case take 3 str of "acc" -> Acc; "jmp" -> Jmp; "nop" -> Nop) num

stepInstr :: (Int -> Instr) -> (Int, Int) -> (Int, Int)
stepInstr ins (cur, acc) = case ins cur of
  Acc n -> (cur + 1, acc + n); Jmp n -> (cur + n, acc); Nop _ -> (cur + 1, acc)

terminate :: V.Vector Instr -> S.IntSet
terminate ins = let
  backMapFor i = M.singleton (fst $ stepInstr (ins V.!) (i, 0)) [i]
  backwards = M.unionsWith (<>) $ backMapFor <$> [0..pred $ V.length ins]
  lookBack f dest = pure dest <> (fold ((M.!?) backwards dest) >>= f)
  in S.fromList $ foldr (const lookBack) pure [0..] $ V.length ins

solP8F :: [String] -> Int
solP8F inp = sol S.empty (0, 0) where
  sol known (i, j)  | i `S.member` known = j
    | otherwise = sol (S.insert i known) $ stepInstr (readInstr inp V.!) (i, j)

solP8S :: [String] -> Int
solP8S inp = sol (0, 0) where
  ins = readInstr inp; termPos = terminate ins
  err (Jmp n) = Nop n; err (Nop n) = Jmp n; err i = i
  getRes = snd . until ((== length ins) . fst) (stepInstr (ins V.!))
  sol p | i' `S.member` termPos = getRes (i', j')
        | otherwise = sol $ stepInstr (ins V.!) p
    where (i', j') = stepInstr (err . (ins V.!)) p