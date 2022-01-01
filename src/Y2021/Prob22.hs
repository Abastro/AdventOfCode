module Y2021.Prob22 ( sol22F, sol22S ) where
import Common
import Data.Char
import Data.List
import qualified Data.MultiSet as MS
import Control.Monad

data Interval a = Interval !a !a deriving (Eq, Ord, Show) -- Interval [a, b]
instance Ord a => Semigroup (Interval a) where -- Provide intersection
  Interval a b <> Interval a' b' = Interval (max a a') (min b b')

nonEmpty int@(Interval a b) = int <$ guard (a <= b)
sizeInter (Interval a b) = b - a + 1

type Cuboid = Vec3 (Interval Int)
data Cmd = Cmd !Bool !Cuboid deriving Show
readCmd :: String -> Cmd -- Repr. region p1 <= X < p2
readCmd s = Cmd (flag == "on") (readI <$> Vec3 ix iy iz) where
  [flag, cuboid] = words s; [ix, iy, iz] = deintercalate ',' cuboid
  readI str = let [a, b] = map read .  words $ map filt str in Interval a b
  filt c = if isDigit c || c == '-' then c else ' '

-- |Runs CMD to apply to addition/subtraction sets (CSG, I believe)
runCmd :: Cmd -> (MS.MultiSet Cuboid, MS.MultiSet Cuboid) -> (MS.MultiSet Cuboid, MS.MultiSet Cuboid)
runCmd (Cmd flag cuboid) (adds, subs) = rmEqual $ if flag then (MS.insert cuboid adds', subs') else (adds', subs')
  where
    rmEqual (a, s) = let eq = MS.intersection a s in (a MS.\\ eq, s MS.\\ eq)
    intWAdd = MS.mapMaybe (traverse nonEmpty . (cuboid <>)) adds;  adds' = MS.union intWSub adds
    intWSub = MS.mapMaybe (traverse nonEmpty . (cuboid <>)) subs;  subs' = MS.union intWAdd subs

sol22 :: [Cmd] -> Int
sol22 cmds = volume $ foldl' (flip runCmd) (mempty, mempty) cmds where
  volOf cuboid = product $ sizeInter <$> cuboid
  volume (adds, subs) = sum (map volOf $ MS.toList adds) - sum (map volOf $ MS.toList subs)

sol22F, sol22S :: [String] -> Int
sol22F l = sol22 cmds where
  cmds = [cmd | cmd@(Cmd _ cuboid) <- readCmd <$> l, all inRange cuboid]
  inRange (Interval a b) = a >= -50 && b <= 50
sol22S l = sol22 $ readCmd <$> l