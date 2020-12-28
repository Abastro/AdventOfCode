module Y2020.Prob14 where

import Data.Foldable ( Foldable(..) )
import Data.Bits ( Bits(..) )
import Data.List ( subsequences )
import qualified Data.IntMap as M

import Text.Read ( Read(..), prec, lexP, choice, pfail, lift )
import Text.Read.Lex ( Lexeme(..), expect )
import qualified Text.ParserCombinators.ReadP as RP

import Common ( liftFn, numToInt )

data BitMask = Z | I | X deriving (Eq, Show)
data Instr = Mask [BitMask] | Mem Int Int deriving Show

instance Read Instr where
  readPrec = prec 0 $ choice [ do
      lift . expect $ Ident "mask"; lift . expect $ Punc "="; lift RP.skipSpaces
      fmap Mask . liftFn RP.many $ lift RP.get >>= interpret
    , do
      lift . expect $ Ident "mem"
      Number pos <- liftFn (RP.between (RP.char '[') (RP.char ']')) lexP
      lift . expect $ Punc "="
      Number n <- lexP
      pure $ Mem (numToInt pos) (numToInt n)
    ] where
      interpret '0' = pure Z
      interpret '1' = pure I
      interpret 'X' = pure X
      interpret _ = pfail

-- NOTE: Lens would've made this much more concise
data ProgState a = ProgState {
  mask0 :: a, mask1 :: Int, memory :: M.IntMap Int
}

perform :: Instr -> ProgState Int -> ProgState Int
perform (Mask mask) st = let
    aided = zip (reverse mask) $ iterate (`shift` 1) 1
    zeros = sum $ map snd $ filter ((/= Z) . fst) aided
    ones = sum $ map snd $ filter ((== I) . fst) aided
  in st{ mask0 = zeros, mask1 = ones }
perform (Mem pos n) st =
  st{ memory = M.insert pos ((n .&. mask0 st) .|. mask1 st) $ memory st }

perform' :: Instr -> ProgState [Int] -> ProgState [Int]
perform' (Mask mask) st = let
    aided = zip (reverse mask) $ iterate (`shift` 1) 1
    flots = map sum $ subsequences $ map snd $ filter ((== X) . fst) aided
    ones = sum $ map snd $ filter ((== I) . fst) aided
  in st{ mask0 = flots, mask1 = ones }
perform' (Mem pos n) st = st{ memory = flip M.union (memory st)
  $ M.fromList $ (, n) . xor (pos .|. mask1 st) <$> mask0 st }

sol1 :: [String] -> Int
sol1 inp = sum . M.elems . memory
  $ foldl' (flip perform) ProgState{ mask0 = 0, mask1 = 0, memory = M.empty }
  $ read <$> inp

sol2 :: [String] -> Int
sol2 inp = sum . M.elems . memory
  $ foldl' (flip perform') ProgState{ mask0 = [], mask1 = 0, memory = M.empty }
  $ read <$> inp
