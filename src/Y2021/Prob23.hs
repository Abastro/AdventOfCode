module Y2021.Prob23 ( sol23F, sol23S ) where
import Common
import Data.Char
import Data.Ord
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import Data.Bits

-- MAYBE - How do I optimize this further?
data Amph = AmA | AmB | AmC | AmD deriving (Eq, Ord, Enum, Bounded, Show) -- cost 10^e
data Hallway = LLsp | Lsp | IAB | IBC | ICD | Rsp | RRsp deriving (Eq, Ord, Enum, Bounded, Show)
data Cfg = Cfg { halls :: !(V.Vector (Maybe Amph)), rooms :: !(V.Vector [Amph]) }
  deriving (Eq, Ord, Show)

cfgAsInt :: Cfg -> Int
cfgAsInt (Cfg h r) = encH + encR `shiftL` 21 where
  encH = sum $ V.imap (\i a -> encM a `shiftL` (i * 3)) h -- 21 bits
  encR = sum $ V.imap (\i a -> encV a `shiftL` (i * 10)) r -- 40 bits
  encM = maybe 4 fromEnum
  encV v = case length v of
    0 -> 0; 1 -> 16 + fromEnum (head v) `shiftL` 2
    l -> sum [fromEnum a `shiftL` (i + i) | (i, a) <- zip [1 ..] v] + pred l
intAsCfg :: Int -> Cfg
intAsCfg n = Cfg (decH $ n .&. maskH) (decR $ n `shiftR` 21) where
  maskH = pred $ 1 `shiftL` 21
  decH m = V.generate 7 $ \i -> decM $ (m `shiftR` (i * 3)) .&. mask where mask = 7
  decR m = V.generate 4 $ \i -> decV $ (m `shiftR` (i * 10)) .&. mask where mask = pred $ 1 `shiftL` 10
  decM m = toEnum m <$ guard (m < 4)
  decV m = if m == 0 then [] else let l = succ (m .&. 3) in
    [toEnum $ (m `shiftR` (i + i)) .&. 3 | i <- [1 .. l]]

readAmphs :: [String] -> V.Vector [Amph]
readAmphs l = V.fromList . getZipList $ traverse (ZipList . toAmph) l where
  toAmph = map (\[c] -> toEnum @Amph (ord c - ord 'A')) . filter (all isAlpha) . deintercalate '#'

costAm am = 10 ^ fromEnum am
crdOf = \case LLsp -> 0; Lsp -> 1; IAB -> 3; IBC -> 5; ICD -> 7; Rsp -> 9; RRsp -> 10
leftOf = \case AmA -> Lsp; AmB -> IAB; AmC -> IBC; AmD -> ICD
distBtwn dep hall room rmln = abs (crdOf hall - (succ . crdOf) (leftOf room)) + (dep - rmln)

towardLR custom halls room = lm <|> rm where
  vat i = V.unsafeIndex halls (fromEnum i)
  lh = leftOf room
  lm = custom vat [lh, pred lh ..];  rm = custom vat [succ lh ..]
isRoomFree rooms room = all (== room) $ V.unsafeIndex rooms (fromEnum room)

-- |Computes possibilities, and give the cost
possNext :: Int -> Cfg -> [(Int, Cfg)]
possNext d (Cfg oldHall oldRoom) = do
  room <- filter (not . isRoomFree oldRoom) [AmA .. AmD]
  case oldRoom V.! fromEnum room of
    [] -> [] -- TODO Calculate next using 'occupied'
    (toMove : rNew) -> map nxtOf . filter consider $ towardLR (\f -> takeWhile (isNothing . f)) oldHall room where
      newRoom = oldRoom V.// [(fromEnum room, rNew)]
      frees = filter (isRoomFree newRoom) [AmA .. AmD]
      nxtOf hall = (costM + cost, stored) where
        newHall = oldHall V.// [(fromEnum hall, Just toMove)]
        costM = costAm toMove * distBtwn d hall room (length rNew)
        (cost, stored) = tryStore d frees (Cfg newHall newRoom)
  where
    occupied = V.toList $ V.imapMaybe (\i v -> needRight (toEnum @Hallway i) <$> v) oldHall
    needRight hall amph = (hall <= leftOf amph, hall)
    (toRight, dived) = minimumBy (comparing divSize) ((True, LLsp) : occupied)
    divSize (flag, hall) = if flag then fromEnum RRsp - fromEnum hall else fromEnum hall
    consider hall = if toRight then hall > dived else hall < dived

-- |Tries storing amphipods capable of going to room
tryStore :: Int -> [Amph] -> Cfg -> (Int, Cfg)
tryStore d freeRooms (Cfg oldHall oldRoom) =
  if null canMoves then (0, Cfg oldHall oldRoom) else (finCost + curCost, finCfg) where
    mayMove room hall = hall <$ guard (oldHall V.! fromEnum hall == Just room)
    getMove room = (, room) <$> towardLR (\f -> mayMove room <=< find (isJust . f)) oldHall room
    canMoves = mapMaybe getMove freeRooms -- For each room, one at a time
    newHall = oldHall V.// [(fromEnum hall, Nothing) | (hall, _) <- canMoves]
    newRoom = V.accum (flip (:)) oldRoom [(fromEnum room, room) | (_, room) <- canMoves]
    (finCost, finCfg) = tryStore d freeRooms (Cfg newHall newRoom)
    costOf (hall, room) = costAm room * distBtwn d hall room (length $ oldRoom V.! fromEnum room)
    curCost = sum $ costOf <$> canMoves

initial = Cfg (V.replicate 7 Nothing)
target d = Cfg (V.replicate 7 Nothing) (V.fromListN 7 $ replicate d <$> [AmA .. AmD])

-- |Traces cost towards the target config
traceCost :: Int -> Int -> (Int, Int) -> S.Set (Int, Int) -> State IS.IntSet Int
traceCost d tar (curP, curV) toVisit = if curV == tar then pure curP else do -- State: (costs, visited)
  visited <- state $ \old -> (old, IS.insert curV old)
  let nexts = S.fromList [(curP + addP, nxtV) | (addP, nxtC) <- possNext d (intAsCfg curV),
                let nxtV = cfgAsInt nxtC, not $ nxtV `IS.member` visited]
  let toVisit' = S.union toVisit nexts
  let handleMin q = case S.minView q of
        Nothing -> error ("Cannot reach " <> show (target d))
        Just ((minP, minV), rem) ->
          if minV `IS.member` visited then handleMin rem
          else traceCost d tar (minP, minV) rem
  handleMin toVisit'

sol23 :: Int -> Cfg -> Int
sol23 depth initial = evalState (traceCost depth (cfgAsInt $ target depth) (0, cfgAsInt initial) mempty) mempty

sol23F, sol23S :: [String] -> Int
sol23F l = sol23 2 inits where
  [_, _, fl, sl, _] = l
  inits = initial $ readAmphs [fl, sl]
sol23S l = sol23 4 inits where
  [_, _, fl, sl, _] = l
  inits = initial $ readAmphs [fl, "  #D#C#B#A#", "  #D#B#A#C#", sl]