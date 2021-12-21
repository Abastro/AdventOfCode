module Y2020.Prob20 ( solP20F, solP20S ) where

import Control.Monad ( guard, foldM )
import Data.Bits ( Bits(..) )
import Data.Foldable ( traverse_ )
import Data.Maybe ( fromMaybe )
import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Common ( deintercalate, count )

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
    , last tile, reverse $ last <$> tile ]) where
    asInt str = sum
      $ zipWith (\i k -> if i == '#' then k else 0) (reverse str)
      $ iterate (`shift` 1) 1


newtype TileConn = TileConn { possible :: M.IntMap [(Int, Orient)] } deriving Show
instance Semigroup TileConn where
  TileConn m <> TileConn m' = TileConn $ M.unionWith (<>) m m'

-- Tiling with normal x,y coordinates
data Tiling a = Tiling { con :: !(M.IntMap a), maxOnDir :: !(V.Vector Int) }
type TileState = (S.IntSet, Tiling (Int, Orient))

pack :: Int -> Int -> Int
pack i j = (i + bit 15) `shift` 16 .|. (j + bit 15)
unpackX :: Int -> Int
unpackX n = subtract (bit 15) $ n `shift` (-16)
unpackY :: Int -> Int
unpackY n = subtract (bit 15) $ n .&. pred (bit 16)

tileAt :: Int -> Int -> Tiling a -> a
tileAt i j = (M.! pack i j) . con

insertAbove :: Dir -> M.IntMap a -> Tiling a -> Tiling a
insertAbove dir row (Tiling c mx) = let
  nd = fromEnum dir; m' = (if dir `elem` [N, E] then succ else pred) $ mx V.! nd
  expand = if dir `elem` [N, S] then (`pack` m') else (m' `pack`)
  in Tiling (c `M.union` M.mapKeysMonotonic expand row) $ mx V.// [(nd, m')]

maxOn :: Dir -> Tiling a -> Int
maxOn dir tile = maxOnDir tile V.! fromEnum dir


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

extendTiling :: M.IntMap TileConn -> Dir -> TileState -> [TileState]
extendTiling conn dir (remain, tiling) = do
  let m = maxOn dir tiling; me = maxOn (E <> dir) tiling; mw = maxOn (W <> dir) tiling
  let lessDir = if mw <= me then W else E
  new <- (`traverse` rangeMap mw me) $ \x -> do -- Gather possible tiles
    let (tile, ori) = tileOn dir x m tiling
    let Orient _ dir' = invert ori <> Orient U dir -- To local coord
    (tile', ori') <- fromMaybe [] $ possible (conn M.! tile) M.!? fromEnum dir'
    guard $ tile' `S.member` remain
    pure (tile', ori <> ori')
  (`traverse_` tail (range mw me)) $ \x -> do -- Weed out unmatching neighbors
    let (tile, ori) = new M.! x
    let Orient _ dir' = invert ori <> Orient U (lessDir <> dir)
    let (tile', relOr) = (invert ori <>) <$> new M.! (x - 1)
    guard $ maybe False ((tile', relOr) `elem`)
      $ possible (conn M.! tile) M.!? fromEnum dir'
  let found = S.fromList $ fst <$> M.elems new
  guard $ S.size found == M.size new -- Need to be all distinct
  pure (remain `S.difference` found, insertAbove dir new tiling)
  where
    tileOn dir x y = if dir `elem` [N, S] then tileAt x y else tileAt y x
    range i j = if i <= j then [i..j] else [j..i]
    rangeMap i j = M.fromAscList $ zip (range i j) (range i j)

findTiling :: [(Int, [[Char]])] -> Tiling (Int, Orient)
findTiling mixed = head $ do
    let tileConn = connectTiles $ fmap tileBounds <$> mixed
    let selected = fst $ M.findMin tileConn
    let init = M.singleton (pack 0 0) (selected, Orient U N) `Tiling` V.replicate 4 0
    let iRem = S.delete selected $ M.keysSet tileConn
    let extend dir ext st = pure st <> (extendTiling tileConn dir st >>= ext)
    let extendPoss dir = foldr extend pure $ repeat dir
    (rem, tiling) <- foldM (flip extendPoss) (iRem, init) [N, S, E, W]
    guard (S.null rem) >> pure tiling

readTiles :: [String] -> [(Int, [[Char]])]
readTiles inp = do
  (header : tile) <- deintercalate [] inp
  pure (read . init $ drop 5 header, tile)

solP20F :: [String] -> Int
solP20F inp = let found = findTiling $ readTiles inp in
  product $ fst <$> (tileAt <$> [maxOn W found, maxOn E found]
    <*> [maxOn S found, maxOn N found] <*> [found])

solP20S :: [String] -> Int
solP20S inp = (S.size sharps -) . (* count '#' (concat seaMonster)) . head . filter (> 0) $ do
    ori <- Orient <$> [U, D] <*> [N, W, S, E]
    pure . length $ do
      ip <- imgIndices
      let cand = S.mapMonotonic (\p -> pack (unpackX ip + unpackX p) (unpackY ip + unpackY p)) (seaMonCoords ori)
      guard $ cand `S.isSubsetOf` sharps
  where
    input = readTiles inp;    found = findTiling input
    size = subtract 2 . length $ snd . head $ input
    stripBnd = tail . init . map (tail . init)
    tiles = M.fromList $ fmap (asMap . stripBnd) <$> input
    tileAtWith ori tile = (tile M.!) . uncurry pack . transformPos size (Orient U W <> invert ori)
    imgAt p = let i = unpackX p;  j = unpackY p
                  (tile, ori) = tileAt (i `div` size) (j `div` size) found
      in tileAtWith ori (tiles M.! tile) (i `mod` size, j `mod` size)
    imgIndices = pack
      <$> [maxOn W found * size .. pred $ (maxOn E found + 1) * size]
      <*> [maxOn S found * size .. pred $ (maxOn N found + 1) * size]
    sharps = S.fromAscList $ filter ((== '#') . imgAt) imgIndices
    -- in matrix coord, xy coord need to be rotated to W
    asMap m = M.fromAscList $ do
      (i, l) <- zip [0..] m; (j, c) <- zip [0..] l; pure (pack i j, c)
    transformPos size ori (i, j) = let (i', j') = transform ori (2*i+1, 2*j+1) in
      (i' `mod` (2*size) `div` 2, j' `mod` (2*size) `div` 2)
    seaMonster = ["                  # "
                , "#    ##    ##    ###"
                , " #  #  #  #  #  #   "]
    seaMonCoords ori = S.fromList $ uncurry pack . transform ori <$> do
      (i, l) <- zip [0..] seaMonster; (j, '#') <- zip [0..] l; pure (i, j)