module Y2021.Prob18 ( sol18F, sol18S ) where
import Common
import Text.Read
import Data.List
import Control.Applicative

data Snail a = RegNum a | PairNum (Snail a) (Snail a) deriving (Eq, Functor, Foldable, Show)
instance Read a => Read (Snail a) where
  readPrec = (RegNum <$> readPrec) <|> (do [l, r] <- readPrec; pure $ PairNum l r)
instance Integral a => Semigroup (Snail a) where
  a <> b = reduceSnail $ PairNum a b

reduceSnail :: Integral a => Snail a -> Snail a -- TODO Need more elegant solution
reduceSnail snail = last $ snail : unfoldr (fmap (\t -> (t, t)) . step) snail where
  step snail = (do (snail', _, _) <- explode 0 snail; pure snail') <|> split snail

  explode depth (PairNum (RegNum a) (RegNum b))
    | depth >= 4 = Just (RegNum 0, Just a, Just b) -- Explodes and produces a, b
  explode depth (PairNum l r) =
    ((`explLeft` r) <$> explode (succ depth) l) <|> ((l `explRight`) <$> explode (succ depth) r)
  explode _ _ = Nothing
  explLeft (l', ma, mb) r = (PairNum l' r', ma, Nothing) where -- TODO Add at r
    r' = maybe r (`addLeft` r) mb
    addLeft n (PairNum l r) = PairNum (addLeft n l) r; addLeft n (RegNum m) = RegNum (n + m)
  explRight l (r', ma, mb) = (PairNum l' r', Nothing, mb) where -- TODO Add at l
    l' = maybe l (`addRight` l) ma
    addRight n (PairNum l r) = PairNum l (addRight n r); addRight n (RegNum m) = RegNum (n + m)

  split (RegNum n) = let (d, r) = n `divMod` 2 in (RegNum d `PairNum` RegNum (d + r)) <$ boolToMaybe (n >= 10)
  split (l `PairNum` r) = ((`PairNum` r) <$> split l) <|> ((l `PairNum`) <$> split r)

mag (PairNum l r) = 3 * mag l + 2 * mag r;    mag (RegNum n) = n

sol18F, sol18S :: [String] -> Int -- TODO Optimize likely needed
sol18F l = mag $ foldl1' (<>) nums where nums = read @(Snail Int) <$> l
sol18S l = maximum $ mag <$> [x <> y | x <- nums, y <- nums, x /= y] where nums = read @(Snail Int) <$> l