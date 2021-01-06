module Y2020.Prob22 ( sol1, sol2 ) where

import Data.Hashable ( Hashable(..) )
import Data.Word ( Word8 )
import qualified Data.HashSet as S
import Data.Vector.Unboxed ( (!) )
import qualified Data.Vector.Unboxed as V
import Common ( deintercalate )

data Deck = Deck { deckFst :: !(V.Vector Word8), deckSnd :: !(V.Vector Word8) } deriving Eq
data WinMark = WinMark { fstWin :: !Bool, wDeck :: !(V.Vector Word8)}
instance Hashable Deck where
  s `hashWithSalt` (Deck v v') = s `hashWithSalt` v!0 `hashWithSalt` v'!0; {-# INLINE hashWithSalt #-}

procWin :: Bool -> Deck -> Deck
procWin fstWin (Deck v v') = let c = v!0; c' = v'!0; cs = V.drop 1 v; cs' = V.drop 1 v' in
  if fstWin then Deck (cs `V.snoc` c `V.snoc` c') cs' else Deck cs (cs' `V.snoc` c' `V.snoc` c)

sol1 :: [String] -> Int
sol1 inp = let
    combat (Deck l l') = procWin (l!0 > l'!0) (Deck l l')
    [x, y] = V.fromList . fmap read . tail <$> deintercalate [] inp
    Deck res1 res2 = until (\(Deck p q) -> V.null p || V.null q) combat $ Deck x y
  in sum $ zipWith (*) [1..] $ reverse $ fromIntegral <$> V.toList (res1 <> res2)

-- TODO Optimize - possible?
sol2 :: [String] -> Int
sol2 inp = let
    reCombat s deck@(Deck v v')
      | V.null v' = WinMark True v   | V.null v = WinMark False v'
      | deck `S.member` s = WinMark True (deckFst deck) -- Same cards same order -> Player 1 wins
      | c < V.length v && c' < V.length v' = reCombat newS $ procWin win deck
      | otherwise = reCombat newS $ procWin (v!0 > v'!0) deck
      where
        c = fromIntegral $ v!0; c' = fromIntegral $ v'!0; cs = V.drop 1 v; cs' = V.drop 1 v'
        newS = s `seq` S.insert deck s
        win = fstWin $ reCombat S.empty $ Deck (V.take c cs) (V.take c' cs')
    [x, y] = V.fromList . fmap read . tail <$> deintercalate [] inp
  in sum $ zipWith (*) [1..] $ reverse . map fromIntegral . V.toList $ wDeck $ reCombat S.empty $ Deck x y