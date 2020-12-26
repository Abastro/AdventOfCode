module Y2020.Prob8 where

import qualified Data.IntMap as M
import qualified Data.IntSet as S

import Data.Maybe ( fromJust, isJust )
import Data.Foldable ( Foldable(..), find )

import Text.Read ( readMaybe )

data Instr = Acc Int | Jmp Int | Nop Int

-- Just too lazy to import and use arrays
readMap :: [String] -> M.IntMap Instr
readMap strs = M.fromAscList $ do
  (i, str) <- zip [0..] strs
  let Just num = readMaybe $ dropWhile (== '+') $ drop 4 str
  (i, ) <$> case take 3 str of
    "acc" -> pure $ Acc num
    "jmp" -> pure $ Jmp num
    "nop" -> pure $ Nop num

stepInstr :: M.IntMap Instr -> (Int, Int) -> (Int, Int)
stepInstr ins (cur, acc) = case ins M.! cur of
  Acc n -> (cur + 1, acc + n)
  Jmp n -> (cur + n, acc)
  Nop _ -> (cur + 1, acc)

terminate :: M.IntMap Instr -> S.IntSet
terminate ins = let
    backwards = M.unionsWith (<>)
      $ (\i -> M.singleton (fst $ stepInstr ins (i, 0)) [i]) <$> M.keys ins
    reachable = iterate $ \dest -> dest >>= fold . (backwards M.!?)
  in S.fromList $ concat $ takeWhile (not . null)
  $ reachable [(+1) . fst $ M.findMax ins]

sol1 :: [String] -> Int
sol1 inp = let ins = readMap inp in
  fromJust . fromJust $ find isJust $ map snd
  $ scanl (\(set, _) (i, j) -> (S.insert i set,
    if S.member i set then Just j else Nothing)) (S.empty, Nothing)
  $ iterate (stepInstr ins) (0, 0)

sol2 :: [String] -> Int
sol2 inp = let
    ins = readMap inp
    lastPos = fst (M.findMax ins) + 1
    termPos = terminate ins
  in snd $ head $ dropWhile ((/= lastPos) . fst)
  $ iterate (stepInstr ins)
  $ fromJust . fromJust $ find isJust
  $ map (\(i, j) -> case ins M.! i of
    Jmp _ -> if S.member (i+1) termPos then Just (i+1, j) else Nothing
    Acc _ -> Nothing
    Nop n -> if S.member (i+n) termPos then Just (i+n, j) else Nothing
    )
  $ iterate (stepInstr ins) (0, 0)
