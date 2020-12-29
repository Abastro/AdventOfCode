module Y2020.Prob8 ( sol1, sol2 ) where

import Data.Maybe ( fromJust, isJust )
import Data.Function ( (&) )
import Data.Foldable ( Foldable(..), find )
import qualified Data.Vector as V
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Common ( boolToMaybe )

data Instr = Acc Int | Jmp Int | Nop Int
readInstr :: [String] -> V.Vector Instr
readInstr strs = V.fromList $ do
  str <- strs; let num = read $ dropWhile (== '+') $ drop 4 str
  pure $ (case take 3 str of "acc" -> Acc; "jmp" -> Jmp; "nop" -> Nop) num

stepInstr :: V.Vector Instr -> (Int, Int) -> (Int, Int)
stepInstr ins (cur, acc) = case ins V.! cur of
  Acc n -> (cur + 1, acc + n); Jmp n -> (cur + n, acc); Nop _ -> (cur + 1, acc)

terminate :: V.Vector Instr -> S.IntSet
terminate ins = let
    backwards = M.unionsWith (<>)
      $ (\i -> M.singleton (fst $ stepInstr ins (i, 0)) [i]) <$> [0..V.length ins-1]
    reachable = iterate $ \dest -> dest >>= fold . (M.!?) backwards
  in S.fromList $ concat $ takeWhile (not . null)
  $ reachable [V.length ins]

sol1 :: [String] -> Int
sol1 inp = let ins = readInstr inp in
  iterate (stepInstr ins) (0, 0)
  & scanl (\(set, _) (i, j) -> (S.insert i set,
    j <$ boolToMaybe (S.member i set))) (S.empty, Nothing)
  & fromJust . fromJust . find isJust . map snd

sol2 :: [String] -> Int
sol2 inp = let ins = readInstr inp; termPos = terminate ins in
  iterate (stepInstr ins) (0, 0)
  & map (\(i, j) -> case ins V.! i of
    Jmp _ -> (i+1, j) <$ boolToMaybe (S.member (i+1) termPos)
    Acc _ -> Nothing
    Nop n -> (i+n, j) <$ boolToMaybe (S.member (i+n) termPos)
    )
  & fromJust . fromJust . find isJust
  & snd . until ((== length ins) . fst) (stepInstr ins)
