module Y2021.Prob25 ( sol25F ) where
import qualified Data.Vector.Unboxed as UV
-- TODO Change to use Mutable
data HMap = HMap { width :: !Int, height :: !Int, hmap :: !(UV.Vector Int) } deriving Eq
mkHMap l = HMap { width = length $ head l, height = length l, hmap = UV.fromList $ map st $ concat l }
  where st '>' = 1; st 'v' = 2; st _ = 0
crdOf (HMap w h _) (x, y) = (x `mod` w) + (y `mod` h) * w

step :: HMap -> HMap
step cur@(HMap w h _) = south where
  east = cur{ hmap = UV.fromList [afterEast cur x y | y <- [0 .. pred h], x <- [0 .. pred w]] }
  afterEast c x y = case hmap c UV.! crdOf c (x, y) of
    0 -> if from == 1 then 1 else 0; 1 -> if to == 0 then 0 else 1; _ -> 2
    where
    from = hmap c UV.! crdOf c (pred x, y)
    to = hmap c UV.! crdOf c (succ x, y)
  south = east{ hmap = UV.fromList [afterSouth east x y | y <- [0 .. pred h], x <- [0 .. pred w]] }
  afterSouth c x y = case hmap c UV.! crdOf c (x, y) of
    0 -> if from == 2 then 2 else 0; 2 -> if to == 0 then 0 else 2; _ -> 1
    where
    from = hmap c UV.! crdOf c (x, pred y)
    to = hmap c UV.! crdOf c (x, succ y)

sol25F :: [String] -> Int
sol25F l = length $ ited (mkHMap l) where
  ited cur = let next = step cur in cur : if next == cur then [] else ited next