{-# OPTIONS_GHC -Wno-missing-methods #-}
module Y2021.Prob19 ( sol19F, sol19S ) where
import Common
import Data.List
import Data.Monoid
import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Control.Monad
import Control.Monad.State

data Axis = XA | YA | ZA deriving (Eq, Ord, Enum)
data Vec3 a = Vec3 { px :: !a, py :: !a, pz :: !a } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance Num a => Num (Vec3 a) where
  fromInteger n = Vec3 (fromInteger n) 0 0;    abs p = abs <$> p
  Vec3 x y z + Vec3 x' y' z' = Vec3 (x + x') (y + y') (z + z')
  Vec3 x y z - Vec3 x' y' z' = Vec3 (x - x') (y - y') (z - z')
type Orient = Vec3 (Bool, Axis) -- False: Negative
type Scanned = V.Vector (Vec3 Int)
type Segments = M.Map (Vec3 Int) (Int, Int)

readScans :: [String] -> V.Vector Scanned
readScans l = V.fromList scans where
  scans = [V.fromList [Vec3 x y z | [x, y, z] <- map read . deintercalate ',' <$> pts] | _ : pts <- deintercalate [] l]

-- |Gives the segments, as map from char into the endpt indices
segments :: Scanned -> Segments
segments scan = M.fromList chars where -- MAYBE filter when a point within cuboid
  chars = [(charOf (scan V.! a - scan V.! b), (a, b)) | b <- [1 .. pred $ V.length scan], a <- [0 .. pred b]]
  charOf p = let l = abs p in Vec3 (minimum l) (maximum l) (sum l)

findMatch :: Segments -> Segments -> (Maybe ((Int, Int), (Int, Int)), IS.IntSet, IS.IntSet)
findMatch seg1 seg2 = (mp, IS.fromList set1, IS.fromList set2) where
  (First mp, set1, set2) = foldMap lists . M.elems $ M.intersectionWith (,) seg1 seg2
  lists p@((a, b), (c, d)) = (First $ Just p, [a, b], [c, d])

-- |Transform from their orientation
orientate :: Orient -> Vec3 Int -> Vec3 Int
orientate ori (Vec3 x y z) = uncurry (\f -> hneg f . crd) <$> ori where
  crd XA = x; crd YA = y; crd ZA = z;   hneg False = negate; hneg True = id

-- |Fit scan2 into scan1's frame of reference, gets scanner position
fitInto :: Scanned -> Scanned -> ((Int, Int), (Int, Int)) -> (IS.IntSet, IS.IntSet) -> (Scanned, Vec3 Int)
fitInto scan1 scan2 ((a, _), (c, c')) (match1, match2) = head $ do
  ori <- oris;  scPos <- [scan1 V.! a - orientate ori (scan2 V.! a') | a' <- [c, c']]
  let attempt = V.map (\p -> scPos + orientate ori p) scan2
  guard $ filtered match1 scan1 == filtered match2 attempt
  pure (attempt, scPos) where
  axes = [Vec3 ax ay az | [ax, ay, az] <- permutations [XA, YA, ZA]]
  oris = axes >>= traverse (\x -> (, x) <$> [False, True])
  filtered m s = S.fromList [s V.! a | a <- IS.toList m]

fitting :: V.Vector Segments -> Int -> State (IS.IntSet, V.Vector (Scanned, Vec3 Int)) () -- State holds yet unfit
fitting segs cur = gets (IS.toList . fst) >>= traverse_ fitFor where
  fitFor next = do
    let (ref, m1, m2) = findMatch (segs V.! cur) (segs V.! next) -- Lazily calculated
    need <- gets $ (next `IS.member`) . fst
    when (need && IS.size m1 >= 12) $ do
      scanned <- gets (V.map fst . snd)
      let Just r = ref; fit = fitInto (scanned V.! cur) (scanned V.! next) r (m1, m2)
      modify' $ \(s, sc) -> (IS.delete next s, sc V.// [(next, fit)])
      fitting segs next -- Next step after reporting

sol19 :: [String] -> V.Vector (Scanned, Vec3 Int) -- Runs in 0.1s, prob easy to get shorter
sol19 l = snd $ execState (fitting segs 0) (idxs, V.map (, 0) scans) where
  scans = readScans l
  segs = V.map segments scans
  idxs = IS.fromList [1 .. pred $ V.length scans]

sol19F, sol19S :: [String] -> Int
sol19F l = S.size $ S.fromList . V.toList . foldMap fst $ sol19 l
sol19S l = maximum pairs where
  pairs = [sum . abs $ poses V.! j - poses V.! i | j <- [0 .. pred $ V.length poses], i <- [0 .. pred j]]
  poses = V.map snd $ sol19 l
