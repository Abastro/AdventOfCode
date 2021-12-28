{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2021.Prob14 ( sol14 ) where
import Common
import Data.Char
import qualified Data.IntMap.Strict as IM

pairList (x, y) = [x, y]
encode a b = ord a * 256 + ord b -- Assumes ASCII characters
ruleMap :: [String] -> IM.IntMap (Int, Int)
ruleMap rs = IM.fromList [(encode a b, (encode a c, encode c b)) | [a, b, ' ', '-', '>', ' ', c] <- rs]

sol14 :: (Int, [String]) -> Int
sol14 (n, l) = maximum occList - minimum occList where
  ini : _ : rs = l; rules = ruleMap rs;   hd = head ini; ls = last ini
  pairOcc = IM.fromListWith (+) $ (, 1) <$> zipWith encode ini (tail ini)
  next occurs = IM.fromListWith (+) newOcc where
    newOcc = [(r, occ) | (pair, occ) <- IM.toList occurs, r <- maybe [pair] pairList (rules IM.!? pair)]
  resPoly = applyN n next pairOcc
  rawOcc = [(ord hd, 1), (ord ls, 1)] <> [(c, occ) | (pair, occ) <- IM.toList resPoly, c <- pairList (pair `divMod` 256)]
  occList = IM.elems $ (`div` 2) <$> IM.fromListWith (+) rawOcc