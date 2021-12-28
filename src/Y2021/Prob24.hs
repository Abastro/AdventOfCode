{-# LANGUAGE StandaloneDeriving #-}
module Y2021.Prob24 ( sol24F, sol24S ) where
import Text.Read
import Text.Printf
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad.State
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import GHC.Exts

-- |Raw Instructions
data RVar = W | X | Y | Z deriving (Eq, Enum, Read, Show)
data RawInstr = INP !RVar | ADD !RVar !ImpV | MUL !RVar !ImpV | DIV !RVar !ImpV | MOD !RVar !ImpV | EQL !RVar !ImpV
  deriving (Eq, Read, Show)
data ImpV = REF !RVar | VAL !Int deriving (Eq, Show)
instance Read ImpV where readPrec = (REF <$> readPrec) <|> (VAL <$> readPrec)

-- |Instructions with constants stored
data Instr = Instr !OpType !RVar !IVal deriving (Eq, Show)
data OpType = Add | Mul | Div | Mod | Eql deriving (Eq, Show)
data IVal = Var !RVar | Con !Int deriving (Eq, Show)

-- |Declarations
data Decl = Op !OpType !Decl !Decl | DV !DVal deriving (Eq)
-- |TCon is true constant, while VCon varies for each step
data DVal = DInp | DState | DRef !Int | TCon !Int | VCon !Int deriving (Eq, Show)
data DGraph = DGraph !Int !(IM.IntMap Decl) [V.Vector Int] deriving (Eq, Show)
instance Show Decl where
  show (Op op lv rv) = printf "(%s %s %s)" (show lv) (str op) (show rv) where
    str Add = "+"; str Mul = "*"; str Div = "/"; str Mod = "%"; str Eql = "=="
  show (DV v) = show v

readInsts :: [String] -> V.Vector RawInstr
readInsts l = V.fromList $ read . map toUpper <$> l

-- NOTE: Assumes that the program repeats save the constants,
-- With z as a sole state and receives 1 input via w at beginning.
refine :: V.Vector RawInstr -> (V.Vector Instr, [V.Vector Int])
refine insts = (the rins, cons) where
  period = let hd = insts V.! 0 in head [i | i <- [1..], V.length insts `mod` i == 0, hd == insts V.! i]
  (reps, 0) = V.length insts `divMod` period
  (rins, cons) = foldMap wrap [refined $ V.slice (i * period) period insts | i <- [0 .. pred reps]]

  refined is = (r, V.fromList $ reverse c) where
    (INP W, is') = (V.head is, V.tail is)
    (r, c) = runState (traverse refOn is') []
  intVal (REF i) = pure (Var i)
  intVal (VAL v) = state $ \s -> (Con $ length s, v : s)
  refOn (INP _) = error "Illegal Input"
  refOn (ADD i v) = Instr Add i <$> intVal v
  refOn (MUL i v) = Instr Mul i <$> intVal v
  refOn (DIV i v) = Instr Div i <$> intVal v
  refOn (MOD i v) = Instr Mod i <$> intVal v
  refOn (EQL i v) = Instr Eql i <$> intVal v
  wrap (r, c) = ([r], [c])

asDecl :: (V.Vector Instr, [V.Vector Int]) -> DGraph
asDecl (ins, cons) = reduced idecl where
  -- Classify Constants
  tcon = head cons
  vcons = IM.fromList $ zip [i | i <- [0 .. pred $ V.length tcon], not . allSame $ (V.! i) <$> cons] [0..]

  -- DGraph Construction
  idecl = DGraph res (IM.fromList $ zip [0..] decls) dvcs where
    iniState = V.fromList [DInp, error "X in state", error "Y in state", DState] -- W, X, Y, Z
    (decls, finState) = runState (traverse declOf . V.toList $ V.indexed ins) iniState
    DRef res = finState V.! fromEnum Z
    dvcs = [V.fromList $ (con V.!) <$> IM.keys vcons | con <- cons]
  declOf (i, Instr op r v) = do
    lv <- gets (V.! fromEnum r); rv <- valOf v
    modify' (V.// [(fromEnum r, DRef i)]); pure $ zeroed op lv rv
  valOf (Var r) = gets (V.! fromEnum r)
  valOf (Con c) = pure $ maybe (TCon $ tcon V.! c) VCon (vcons IM.!? c)
  zeroed Mul _ (TCon 0) = DV (TCon 0) -- Detect decl being zeroed
  zeroed op lv rv = Op op (DV lv) (DV rv)

  -- DGraph Optimization
  reduced (DGraph res decs dvcs) = DGraph (pred $ IM.size newDecs) newDecs dvcs where
    nRevLink = IM.fromListWith (+) $ (, 1) <$> foldMap links decs
    links (Op _ lv rv) = links lv <> links rv
    links (DV (DRef i)) = [i]; links _ = []

    newDecs = IM.fromList $ zip [0..] ((snd <$> IM.elems ndecs) <> [ndr]) where
      (ndr, ndecs) = runState (red $ decs IM.! res) IM.empty
    red (Op op lv rv) = simpl op <$> red lv <*> red rv
    red (DV (DRef i)) = gets (fmap fst . (IM.!? i)) >>= \case
      Just i' -> pure (DV (DRef i'))
      Nothing -> do
        r <- red (decs IM.! i)
        if nRevLink IM.!? i <= Just 1 then pure r
        else state $ \m -> let i' = IM.size m in (DV (DRef i'), IM.insert i (i', r) m)
    red (DV v) = pure (DV v)

    simpl Add (DV (TCon 0)) r = r; simpl Add l (DV (TCon 0)) = l
    simpl op l r = Op op l r

  allSame (x : l) = all (== x) l; allSame [] = True

-- MAYBE Generalized stack machine
extract :: DGraph -> [(Int, Int, Int)]
extract (DGraph res decs dvcs) = if decs == eqCode && res == 1 then exStack else error "Illegal code" where
  eqCode = IM.fromList [(0, nflag), (1, pushed)] where
    seek = Op Mod (DV DState) (DV $ TCon 26)
    flag = Op Eql (Op Add seek (DV $ VCon 1)) (DV DInp)
    nflag = Op Eql flag (DV $ TCon 0)

    mayPop = Op Div (DV DState) (DV $ VCon 0)
    nfMul = Op Add (Op Mul (DV $ TCon 25) (DV $ DRef 0)) (DV $ TCon 1)
    nfAdd = Op Mul (Op Add (DV DInp) (DV $ VCon 2)) (DV $ DRef 0)
    pushed = Op Add (Op Mul mayPop nfMul) nfAdd

  exStack = res where -- Paired inputs' difference
    (res, []) = runState (concat <$> traverse (uncurry onCons) cons) []
    cons = zip [0..] $ V.toList <$> dvcs

    onCons i [1, ux, v] | v >= 0 && 9 + v < 26 && ux >= 10 =
      state $ \s -> ([], (i, v) : s)
    onCons j [26, u, vx] | vx >= 0 && 9 + vx < 26 =
      state $ \((i, v) : s) -> ([(i, j, u + v)], s)
    onCons _ _ = error "Unexpected constant"


sol24F, sol24S :: [String] -> Int
sol24F l = V.foldl' (\m d -> m * 10 + d) 0 large where
  decls@(DGraph _ _ cs) = asDecl . refine $ readInsts l
  reqMargin = IM.fromList [if d >= 0 then (i, d) else (j, -d) | (i, j, d) <- extract decls]
  large = V.generate (length cs) (\i -> (9 -) . fromMaybe 0 $ reqMargin IM.!? i)

sol24S l = V.foldl' (\m d -> m * 10 + d) 0 small where
  decls@(DGraph _ _ cs) = asDecl . refine $ readInsts l
  reqMargin = IM.fromList [if d >= 0 then (j, d) else (i, -d) | (i, j, d) <- extract decls]
  small = V.generate (length cs) (\i -> (1 +) . fromMaybe 0 $ reqMargin IM.!? i)