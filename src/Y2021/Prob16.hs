{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2021.Prob16 ( sol16F, sol16S ) where
import Data.Tuple
import Data.List
import Control.Monad.RWS

data Bit = O | I deriving (Eq, Enum, Show)
toBits l = fmap (toEnum @Bit) . reverse . take l . unfoldr (\n -> Just $ swap (n `divMod` 2))
fromBits = foldl' @[] (\n b -> n * 2 + b) 0 . fmap (fromEnum @Bit)
hexToBit s = [b | c <- s, c /= '\n', b <- toBits 4 . read @Int $ "0x" <> [c]]

type ReadBits = RWS () (Sum Int) [Bit]
readN n = tell (Sum n) *> state (splitAt n)

data Packet = Packet !Int !Content deriving Show
data Content = Literal !Int | Operator !Int [Packet] deriving Show
readPacket :: ReadBits Packet -- also gives read bit size
readPacket = do
  ver <- fromBits <$> readN 3
  typ <- fromBits <$> readN 3
  Packet ver <$> if typ == 4
    then Literal . fromBits <$> readLong
    else Operator typ <$> readSubs
  where
    readLong = readN 5 >>= \case I:v -> (v <>) <$> readLong; O:v -> pure v
    readTill 0 = pure []
    readTill m = do (p, Sum m') <- listens (Sum m -) readPacket; (p :) <$> readTill m'
    readSubs = readN 1 >>= \case
      [O] -> readN 15 >>= readTill . fromBits
      [I] -> readN 11 >>= (`replicateM` readPacket) . fromBits

sol16F :: String -> Int
sol16F s = sumVer packet where
  sumVer (Packet v c) = v + sumVerC c
  sumVerC (Operator _ ps) = sum $ sumVer <$> ps; sumVerC (Literal _) = 0
  (packet, _, _) = runRWS readPacket () (hexToBit s)

sol16S :: String -> Int
sol16S s = calc packet where
  calc (Packet _ (Operator n ps)) = op n $ calc <$> ps;   calc (Packet _ (Literal n)) = n
  op 0 = sum; op 1 = product; op 2 = minimum; op 3 = maximum
  op 5 = \[m, n] -> fromEnum (m > n); op 6 = \[m, n] -> fromEnum (m < n); op 7 = \[m, n] -> fromEnum (m == n)
  (packet, _, _) = runRWS readPacket () (hexToBit s)