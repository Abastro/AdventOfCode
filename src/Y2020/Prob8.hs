module Y2020.Prob8 where

import qualified Data.Vector as V
import qualified Data.IntMap as M
import qualified Data.IntSet as S

import Data.Maybe ( fromJust, isJust )
import Data.Function ( (&) )
import Data.Foldable ( Foldable(..), find )

data Instr = Acc Int | Jmp Int | Nop Int

-- Too lazy to import and use vectors
readMap :: [String] -> V.Vector Instr
readMap strs = V.fromList $ do
  str <- strs
  let num = read $ dropWhile (== '+') $ drop 4 str
  pure $ case take 3 str of
    "acc" -> Acc num
    "jmp" -> Jmp num
    "nop" -> Nop num

stepInstr :: V.Vector Instr -> (Int, Int) -> (Int, Int)
stepInstr ins (cur, acc) = case ins V.! cur of
  Acc n -> (cur + 1, acc + n)
  Jmp n -> (cur + n, acc)
  Nop _ -> (cur + 1, acc)

terminate :: V.Vector Instr -> S.IntSet
terminate ins = let
    backwards = M.unionsWith (<>)
      $ (\i -> M.singleton (fst $ stepInstr ins (i, 0)) [i]) <$> [0..V.length ins-1]
    reachable = iterate $ \dest -> dest >>= fold . (M.!?) backwards
  in S.fromList $ concat $ takeWhile (not . null)
  $ reachable [V.length ins]

sol1 :: [String] -> Int
sol1 inp = let ins = readMap inp in
  iterate (stepInstr ins) (0, 0)
  & scanl (\(set, _) (i, j) -> (S.insert i set,
    if S.member i set then Just j else Nothing)) (S.empty, Nothing)
  & fromJust . fromJust . find isJust . map snd

sol2 :: [String] -> Int
sol2 inp = let
    ins = readMap inp
    lastPos = length ins
    termPos = terminate ins
  in iterate (stepInstr ins) (0, 0)
  & map (\(i, j) -> case ins V.! i of
    Jmp _ -> if S.member (i+1) termPos then Just (i+1, j) else Nothing
    Acc _ -> Nothing
    Nop n -> if S.member (i+n) termPos then Just (i+n, j) else Nothing
    )
  & fromJust . fromJust . find isJust
  & snd . until ((== lastPos) . fst) (stepInstr ins)
