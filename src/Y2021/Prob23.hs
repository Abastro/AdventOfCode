module Y2021.Prob23 ( sol23F, sol23S ) where
import Common
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import Data.Bits

-- TODO More optimization required
data Amph = AmA | AmB | AmC | AmD deriving (Eq, Ord, Enum, Bounded, Show) -- cost 10^e
data Hallway = LLsp | Lsp | IAB | IBC | ICD | Rsp | RRsp deriving (Eq, Ord, Enum, Bounded, Show)
data Cfg = Cfg { halls :: !(V.Vector (Maybe Amph)), rooms :: !(V.Vector [Amph]) }
  deriving (Eq, Ord, Show)

cfgAsInt :: Cfg -> Int
cfgAsInt (Cfg h r) = encH + encR `shiftL` 21 where
  encH = sum $ V.imap (\i a -> encM a `shiftL` (i * 3)) h -- 21 bits
  encR = sum $ V.imap (\i a -> encV a `shiftL` (i * 10)) r -- 40 bits
  encM = maybe 4 fromEnum
  encV v = if null v then 0 else let l = length v in
    sum [fromEnum a `shiftL` (i + i) | (i, a) <- zip [0 ..] v] + pred l `shiftL` 8 + (if l == 1 then 4 else 0)
intAsCfg :: Int -> Cfg
intAsCfg n = Cfg (decH $ n .&. maskH) (decR $ n `shiftR` 21) where
  maskH = pred $ 1 `shiftL` 21
  decH m = V.generate 7 $ \i -> decM $ (m `shiftR` (i * 3)) .&. mask where mask = 7
  decR m = V.generate 4 $ \i -> decV $ (m `shiftR` (i * 10)) .&. mask where mask = pred $ 1 `shiftL` 10
  decM m = toEnum m <$ boolToMaybe (m < 4)
  decV m = if m == 0 then [] else let l = succ (m `shiftR` 8) in
    [toEnum $ (m `shiftR` (i + i)) .&. 3 | i <- [0 .. pred l]]

readAmphs :: [String] -> V.Vector [Amph]
readAmphs l = V.fromList . getZipList $ traverse (ZipList . toAmph) l where
  toAmph = map (\[c] -> toEnum @Amph (ord c - ord 'A')) . filter (all isAlpha) . deintercalate '#'

costAm am = 10 ^ fromEnum am
crdOf = \case LLsp -> 0; Lsp -> 1; IAB -> 3; IBC -> 5; ICD -> 7; Rsp -> 9; RRsp -> 10
leftOf AmA = Lsp; leftOf AmB = IAB; leftOf AmC = IBC; leftOf AmD = ICD
distBtwn dep hall room rmst = abs (crdOf hall - (succ . crdOf) (leftOf room)) + (dep - length rmst)
interval a b = case a `compare` b of LT -> [a, succ a .. b]; GT -> [a, pred a .. b]; EQ -> [a]

leftNRight prod halls room = lefts <|> rights where
  hallAt i = halls V.! fromEnum i
  lefts = prod hallAt $ interval (leftOf room) minBound
  rights = prod hallAt $ interval (succ $ leftOf room) maxBound
isRoomFree rooms room = all (== room) $ rooms V.! fromEnum room

-- |Computes possibilities, and give the cost
possNext :: Int -> Cfg -> [(Int, Cfg)] -- TODO fix duplicate cfg states by putting some directly
possNext d (Cfg oldHall oldRoom) = do
  room <- filter (not . isRoomFree oldRoom) [AmA .. AmD]
  case oldRoom V.! fromEnum room of
    [] -> []; (toMove : rNew) -> do
      hall <- leftNRight (takeWhile . (isNothing .)) oldHall room
      let newHall = oldHall V.// [(fromEnum hall, Just toMove)]
          newRoom = oldRoom V.// [(fromEnum room, rNew)]
          costM = costAm toMove * distBtwn d hall room rNew
          (cost, stored) = tryStore d (Cfg newHall newRoom)
      pure (costM + cost, stored)

-- |Tries storing amphipods capable of going to room
tryStore :: Int -> Cfg -> (Int, Cfg)
tryStore d cfg@(Cfg oldHall oldRoom) =
  if null canMoves then (0, cfg) else (finCost + curCost, finCfg) where
    mayMove room v t = do hall <- find (isJust . v) t; (hall, room) <$ boolToMaybe (v hall == Just room)
    getMove room = leftNRight (mayMove room) oldHall room
    canMoves = mapMaybe getMove $ filter (isRoomFree oldRoom) [AmA .. AmD] -- For each room, one at a time
    newHall = oldHall V.// [(fromEnum hall, Nothing) | (hall, _) <- canMoves]
    newRoom = V.accum (flip (:)) oldRoom [(fromEnum room, room) | (_, room) <- canMoves]
    (finCost, finCfg) = tryStore d (Cfg newHall newRoom)
    costOf (hall, room) = costAm room * distBtwn d hall room (oldRoom V.! fromEnum room)
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