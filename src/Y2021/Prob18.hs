module Y2021.Prob18 ( sol18F, sol18S ) where
import Common
import Text.Read
import Data.List
import Control.Applicative

data Snail a = RegNum !a | PairNum !(Snail a) !(Snail a) deriving (Eq, Functor, Foldable, Show)
instance Read a => Read (Snail a) where
  readPrec = (RegNum <$> readPrec) <|> (do [l, r] <- readPrec; pure $ PairNum l r)
instance (Integral a, Show a) => Semigroup (Snail a) where
  a <> b = reduceSnail $ PairNum a b

reduceSnail :: (Integral a, Show a) => Snail a -> Snail a
reduceSnail snail = maybe snail reduceSnail $ step snail where -- MAYBE Optimize
  step snail = ((\(snail', _, _) -> snail') <$> explode 0 snail) <|> split snail
  explode depth (PairNum (RegNum a) (RegNum b))
    | depth >= 4 = Just (RegNum 0, a, b) -- Explodes and produces a, b
  explode depth (PairNum l r) =
    ((`explLeft` r) <$> explode (succ depth) l) <|> ((l `explRight`) <$> explode (succ depth) r)
  explode _ _ = Nothing
  explLeft (l', ma, mb) r = (PairNum l' r', ma, 0) where r' = addLeft mb r
  explRight l (r', ma, mb) = (PairNum l' r', 0, mb) where l' = addRight ma l

  split (RegNum n) = let (d, r) = n `divMod` 2 in (RegNum d `PairNum` RegNum (d + r)) <$ boolToMaybe (n >= 10)
  split (l `PairNum` r) = ((`PairNum` r) <$> split l) <|> ((l `PairNum`) <$> split r)

  addLeft n (PairNum l r) = PairNum (addLeft n l) r; addLeft n (RegNum m) = RegNum (n + m)
  addRight n (PairNum l r) = PairNum l (addRight n r); addRight n (RegNum m) = RegNum (n + m)

mag (PairNum l r) = 3 * mag l + 2 * mag r;    mag (RegNum n) = n
sol18F, sol18S :: [String] -> Int
sol18F l = mag $ foldl1' (<>) nums where nums = reduceSnail . read @(Snail Int) <$> l
sol18S l = maximum $ mag <$> [x <> y | x <- nums, y <- nums, x /= y] where nums = reduceSnail . read @(Snail Int) <$> l