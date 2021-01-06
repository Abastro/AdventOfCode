module Y2020.Prob20 ( sol1, sol2 ) where

import Control.Monad ( guard, foldM )
import Data.Bits ( shift )
import Data.List ( find )
import Data.Foldable ( traverse_ )
import Data.Maybe ( fromMaybe, fromJust )
import qualified Data.HashSet as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as M
import Common ( deintercalate )

data Dir = N | W | S | E deriving (Eq, Ord, Enum, Show)
-- Describes LR-flip
data Face = U | D deriving (Eq, Ord, Enum, Show)
-- Flip-Rotate (i.e. Direction denotes N side); Has D8 group structure
data Orient = Orient { face :: Face, dir :: Dir } deriving (Eq, Ord, Show)
instance Semigroup Dir where
  dir <> dir' = toEnum $ (fromEnum dir + fromEnum dir') `mod` 4
instance Semigroup Face where
  f <> f' = toEnum $ (fromEnum f + fromEnum f') `mod` 2
instance Semigroup Orient where
  Orient f d <> Orient f' d' = Orient (f <> f') (d <> flipTo f d')

-- Inverts Orient in group sense
invert :: Orient -> Orient
invert (Orient f dir) = Orient f (flipTo (f <> D) dir)
flipTo :: Face -> Dir -> Dir
flipTo U dir = dir; flipTo D dir = toEnum $ (- fromEnum dir) `mod` 4
transform :: Orient -> (Int, Int) -> (Int, Int)
transform (Orient D dir) (x, y) = transform (Orient U dir) (-x, y)
transform (Orient U dir) c = rotate dir c where
  rotate N (x, y) = (x, y);   rotate W (x, y) = (-y, x)
  rotate S (x, y) = (-x, -y); rotate E (x, y) = (y, -x)

tileBounds :: [[Char]] -> [(Orient, Int)]
tileBounds tile =
  -- Find ori: Hypothetical (w/ Northside match) ={ori}> Read Tile
  zip (Orient <$> [U, D] <*> [N, W, S, E]) $ asInt
  <$> ([id, reverse] <*> [ reverse $ head tile, head <$> tile
    , last tile, reverse $ last <$> tile ])
  where
    asInt str = sum
      $ zipWith (\i k -> if i == '#' then k else 0) (reverse str)
      $ iterate (`shift` 1) 1

newtype TileConn = TileConn { possible :: M.IntMap [(Int, Orient)] } deriving Show
instance Semigroup TileConn where
  TileConn m <> TileConn m' = TileConn $ M.unionWith (<>) m m'

connectTiles :: [(Int, [(Orient, Int)])] -> M.IntMap TileConn
connectTiles tiles = let
  gather = M.elems . M.fromListWith (<>) $ fmap pure <$> do
    (tile, conn) <- tiles; (ori, bnd) <- conn
    pure (bnd, (ori, tile))
  -- Prevent connection to self, and discard flipped ones
  conn (ori, tile) (ori', tile') = if face ori == D || tile == tile' then M.empty else
    M.singleton tile $ TileConn $ M.singleton (fromEnum $ dir ori)
    -- Make it 'flip to south' and rotate to fit
      [(tile', ori <> Orient D S <> invert ori')]
  in M.unionsWith (<>) $ do list <- gather; conn <$> list <*> list

-- Tiling with normal x,y coordinates
type Tiling a = M.IntMap (M.IntMap a)
type TileState = (IS.IntSet, Tiling (Int, Orient))

tileAt :: Int -> Int -> Tiling a -> a
tileAt i j = (M.! j) . (M.! i)

insertAbove :: Dir -> Int -> M.IntMap a -> Tiling a -> Tiling a
insertAbove N i row = M.mapWithKey (\j -> M.insert (i+1) $ row M.! j)
insertAbove S i row = M.mapWithKey (\j -> M.insert (i-1) $ row M.! j)
insertAbove E i row = M.insert (i+1) row
insertAbove W i row = M.insert (i-1) row

maxOn :: Dir -> Tiling a -> Int
maxOn N tile = fst $ M.findMax $ snd . M.findMax $ tile
maxOn S tile = fst $ M.findMin $ snd . M.findMax $ tile
maxOn E tile = fst $ M.findMax tile
maxOn W tile = fst $ M.findMin tile

extendTiling :: M.IntMap TileConn -> Dir -> TileState -> [TileState]
extendTiling conn dir (remain, tiling) = do
  let m = maxOn dir tiling
  let me = maxOn (E <> dir) tiling
  let mw = maxOn (W <> dir) tiling
  let lessDir = if mw <= me then W else E
  new <- (`traverse` rangeMap mw me) $ \x -> do -- Gather possible tiles
    let (tile, ori) = tileOn dir x m tiling
    let Orient _ dir' = invert ori <> Orient U dir -- To local coord
    (tile', ori') <- fromMaybe [] $ possible (conn M.! tile) M.!? fromEnum dir'
    guard $ tile' `IS.member` remain
    pure (tile', ori <> ori')
  (`traverse_` tail (range mw me)) $ \x -> do -- Weed out unmatching neighbors
    let (tile, ori) = new M.! x
    let Orient _ dir' = invert ori <> Orient U (lessDir <> dir)
    let (tile', relOr) = (invert ori <>) <$> new M.! (x - 1)
    guard $ maybe False (elem (tile', relOr))
      $ possible (conn M.! tile) M.!? fromEnum dir'
  let found = IS.fromList $ fst <$> M.elems new
  guard $ IS.size found == M.size new -- Need to be all distinct
  pure (remain `IS.difference` found, insertAbove dir m new tiling)
  where
    tileOn dir i j = if dir `elem` [N, S] then tileAt i j else tileAt j i
    range i j = if i <= j then [i..j] else [j..i]
    rangeMap i j = M.fromAscList $ zip (range i j) (range i j)

findTiling :: [(Int, [[Char]])] -> Tiling (Int, Orient)
findTiling mixed = let tileConn = connectTiles $ fmap tileBounds <$> mixed in
  head $ do
    let selected = fst $ M.findMin tileConn
    let initial = M.singleton 0 $ M.singleton 0 (selected, Orient U N)
    let remain = IS.delete selected $ M.keysSet tileConn
    let extend dir = (>>= extendTiling tileConn dir)
    let extendPoss dir = takeWhile (not . null) . iterate (extend dir)
    poss <- foldM (flip extendPoss) (pure (remain, initial)) [N, S, E, W]
    (rem, tiling) <- poss
    guard (IS.null rem) >> pure tiling

readTiles :: [String] -> [(Int, [[Char]])]
readTiles inp = do
  (header : tile) <- deintercalate [] inp
  pure (read . init $ drop 5 header, tile)

sol1 :: [String] -> Int
sol1 inp = let found = findTiling $ readTiles inp in
  product $ fst <$> (tileAt <$> [maxOn W found, maxOn E found]
    <*> [maxOn S found, maxOn N found] <*> [found])

sol2 :: [String] -> Int
sol2 inp = let
  size = subtract 2 . length $ snd . head $ readTiles inp
  stripBnd = tail . init . map (tail . init)
  tiles = M.fromList $ fmap (asMap . stripBnd) <$> readTiles inp
  tileAtWith ori = flip $ uncurry tileAt
    . transformPos size (Orient U W <> invert ori)
  sorted = (fmap . fmap) (\(tile, ori') -> tileAtWith ori' (tiles M.! tile))
    $ findTiling $ readTiles inp
  imgAt (i, j) = tileAt (i `div` size) (j `div` size) sorted (i `mod` size, j `mod` size)
  imgIndices = (,) <$> [maxOn W sorted * size .. pred $ (maxOn E sorted + 1) * size]
    <*> [maxOn S sorted * size .. pred $ (maxOn N sorted + 1) * size]
  sharps = S.fromList $ filter ((== '#') . imgAt) imgIndices
  in (S.size sharps -) . (* S.size seaMonCoords) . fromJust . find (> 0) $ do
    ori <- Orient <$> [U, D] <*> [N, W, S, E]
    pure . length $ do
      (x, y) <- imgIndices
      let cand = S.map (\(p, q) -> (p + x, q + y)) $ S.map (transform ori) seaMonCoords
      guard (cand `S.isSubsetOf` sharps)
  where
    -- in matrix coord, xy coord need to be rotated to W
    asMap = M.fromList . zip [0..] . map (M.fromList . zip [0..])
    transformPos size ori (i, j) = let (i', j') = transform ori (2*i+1, 2*j+1) in
      (i' `mod` (2*size) `div` 2, j' `mod` (2*size) `div` 2)
    seaMonster = ["                  # "
                , "#    ##    ##    ###"
                , " #  #  #  #  #  #   "]
    seaMonCoords = S.fromList $ do
      (i, l) <- zip [0 :: Int ..] seaMonster; (j, '#') <- zip [0 :: Int ..] l
      pure (i, j)
