module Y2021.Prob23 ( sol23F, sol23S ) where
import Common
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State

data Amph = AmA | AmB | AmC | AmD deriving (Eq, Ord, Enum, Bounded, Show) -- cost 10^e
data Hallway = LLsp | Lsp | IAB | IBC | ICD | Rsp | RRsp deriving (Eq, Ord, Enum, Bounded, Show)
data Cfg = Cfg { depth :: Int, halls :: !(V.Vector (Maybe Amph)), rooms :: !(V.Vector (V.Vector Amph)) }
  deriving (Eq, Ord, Show)

readAmphs :: [String] -> V.Vector (V.Vector Amph)
readAmphs l = V.fromList . getZipList $ traverse (ZipList . toAmph) $ V.fromList l where
  toAmph = map (\[c] -> toEnum @Amph (ord c - ord 'A')) . filter (all isAlpha) . deintercalate '#'

costAm am = 10 ^ fromEnum am
crdOf = \case LLsp -> 0; Lsp -> 1; IAB -> 3; IBC -> 5; ICD -> 7; Rsp -> 9; RRsp -> 10
leftOf AmA = Lsp; leftOf AmB = IAB; leftOf AmC = IBC; leftOf AmD = ICD
distBtwn dep hall room rmst = abs (crdOf hall - (succ . crdOf) (leftOf room)) + (dep - length rmst)

leftNRight prod halls room = lefts <|> rights where
  hallAt i = halls V.! fromEnum i
  interval a b = case a `compare` b of LT -> [a, succ a .. b]; GT -> [a, pred a .. b]; EQ -> [a]
  lefts = prod hallAt $ interval (leftOf room) minBound
  rights = prod hallAt $ interval (succ $ leftOf room) maxBound

-- |Computes possibilities, and give the cost
possNext :: StateT Cfg [] Int
possNext = do
  d <- gets depth
  room <- lift [AmA .. AmD]; rOld <- gets ((V.! fromEnum room) . rooms)
  (toMove, rNew) <- lift . maybeToList $ uncons (V.toList rOld) <* boolToMaybe (any (/= room) rOld)
  oldHall <- gets halls; hall <- lift $ leftNRight (takeWhile . (isNothing .)) oldHall room
  modify' $ \(Cfg d h r) ->
    Cfg d (h V.// [(fromEnum hall, Just toMove)]) (r V.// [(fromEnum room, V.fromList rNew)])
  let costM = costAm toMove * distBtwn d hall room rNew
  (costM +) <$> state tryStore

-- |Tries storing some amphipods
tryStore :: Cfg -> (Int, Cfg)
tryStore cfg@(Cfg d oldHall oldRoom) =
  if null canMoves then (0, cfg) else (finCost + curCost, finCfg) where
    freeRoom = [room | room <- [AmA .. AmD], all (== room) $ oldRoom V.! fromEnum room]
    mayMove room v t = do hall <- find (isJust . v) t; (hall, room) <$ boolToMaybe (v hall == Just room)
    getMove room = leftNRight (mayMove room) oldHall room
    canMoves = mapMaybe getMove freeRoom -- For each room, one at a time
    newHall = oldHall V.// [(fromEnum hall, Nothing) | (hall, _) <- canMoves]
    newRoom = V.accum (flip V.cons) oldRoom [(fromEnum room, room) | (_, room) <- canMoves]
    (finCost, finCfg) = tryStore (Cfg d newHall newRoom)
    costOf (hall, room) = costAm room * distBtwn d hall room (oldRoom V.! fromEnum room)
    curCost = sum $ costOf <$> canMoves

initial d = Cfg d (V.replicate 7 Nothing)
target d = Cfg d (V.replicate 7 Nothing) (V.fromList $ V.replicate d <$> [AmA .. AmD])
-- |Traces cost towards the target config
traceCost :: (Int, Cfg) -> S.Set (Int, Cfg) -> State (M.Map Cfg Int, S.Set Cfg) Int
traceCost (cost, cfg) toVisit = if cfg == tar then pure cost else do -- State: (costs, visited)
  visited <- state $ \(c, old) -> (old, (c, S.insert cfg old))
  let nexts = filter ((`S.notMember` visited) . snd) $ runStateT possNext cfg
  visUp <- traverse updateCost nexts
  let toVisit' = foldl' updateVis toVisit visUp
  case S.minView toVisit' of
    Nothing -> error (show tar <> ": Not Reachable")
    Just (nextV, nextVisit) -> traceCost nextV nextVisit
  where
    tar = target (depth cfg)
    updateCost (incr, cfg) = state $ \(old, v) ->
      let new = M.insertWith min cfg (cost + incr) old
      in ((cfg, (fromMaybe maxBound $ old M.!? cfg, new M.! cfg)), (new, v))
    updateVis toVisit (cfg, (old, new)) = S.insert (new, cfg) . S.delete (old, cfg) $ toVisit

sol23F :: [String] -> Int
sol23F l = evalState (traceCost (0, inits) S.empty) (M.empty, S.empty) where
  [_, _, fl, sl, _] = l
  inits = initial 2 $ readAmphs [fl, sl]

sol23S :: [String] -> Int -- TODO Heavy optimization required
sol23S l = evalState (traceCost (0, inits) S.empty) (M.empty, S.empty) where
  [_, _, fl, sl, _] = l
  inits = initial 4 $ readAmphs [fl, "  #D#C#B#A#", "  #D#B#A#C#", sl]