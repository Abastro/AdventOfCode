module Common
  ( c2word
  , inRange
  , (...)
  , applyN
  , count
  , deintercalate
  , liftFn
  , fromDigit
  , Vec2(..)
  , Vec3(..)
  , Frame(..)
  , Framed(..)
  , mkFrame
  , mkFramed
  , mkFramedF
  , framedThaw
  , inFrame
  , frameIdx
  , fromIdx
  , getAt
  , frameCrds
  , Enumed(..)
  , genEnumed
  , enumGet
  , enumSets
  , enumModifies
  ) where

import           Control.Monad.ST
import           Data.Char                      ( ord )
import           Data.Function                  ( on )
import           Data.List                      ( foldl'
                                                , groupBy
                                                )
import qualified Data.Vector.Generic           as V
--import qualified Data.Vector.Generic.Mutable   as MV
import           Data.Word                      ( Word8 )
import           Text.ParserCombinators.ReadP   ( ReadP )
import           Text.ParserCombinators.ReadPrec

c2word :: Char -> Word8 -- Unsafe
c2word = fromIntegral . ord

inRange :: (Int, Int) -> Int -> Bool
inRange (lb, ub) = (&&) <$> (>= lb) <*> (< ub)

(...) :: (Num a, Enum a) => a -> a -> [a]
x ... y = [x, x + signum (y - x) .. y]

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = go n x where
  go 0 x = x
  go n x = let x' = f x in x' `seq` go (pred n) x'

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

deintercalate :: Eq a => a -> [a] -> [[a]]
deintercalate p = filter (/= [p]) . groupBy ((==) `on` (== p))

liftFn :: (ReadP a -> ReadP b) -> (ReadPrec a -> ReadPrec b)
liftFn f = readP_to_Prec . (f .) . readPrec_to_P

fromDigit :: Int -> [Int] -> Int
fromDigit base = foldl' (\n d -> n * base + d) 0
{-# INLINE fromDigit #-}

data Vec2 a = Vec2
  { px :: !a
  , py :: !a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance Num a => Num (Vec2 a) where
  Vec2 x y + Vec2 x' y' = Vec2 (x + x') (y + y')
  Vec2 x y - Vec2 x' y' = Vec2 (x - x') (y - y')
  _ * _ = error "illegal"
  fromInteger _ = error "illegal"
  abs _ = error "illegal"
  signum _ = error "illegal"
instance Semigroup a => Semigroup (Vec2 a) where
  Vec2 x y <> Vec2 x' y' = Vec2 (x <> x') (y <> y')
instance Monoid a => Monoid (Vec2 a) where
  mempty = Vec2 mempty mempty

data Vec3 a = Vec3
  { sx :: !a
  , sy :: !a
  , sz :: !a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance Num a => Num (Vec3 a) where
  Vec3 x y z + Vec3 x' y' z' = Vec3 (x + x') (y + y') (z + z')
  Vec3 x y z - Vec3 x' y' z' = Vec3 (x - x') (y - y') (z - z')
  _ * _ = error "illegal"
  fromInteger _ = error "illegal"
  abs _ = error "illegal"
  signum _ = error "illegal"
instance Semigroup a => Semigroup (Vec3 a) where
  Vec3 x y z <> Vec3 x' y' z' = Vec3 (x <> x') (y <> y') (z <> z')
instance Monoid a => Monoid (Vec3 a) where
  mempty = Vec3 mempty mempty mempty

data Frame = Frame
  { width  :: !Int
  , height :: !Int
  }
  deriving (Eq, Ord, Show)
-- |2D vector
data Framed v a = Framed
  { frame :: !Frame
  , umap  :: !(v a)
  }
  deriving (Eq, Ord, Show)

mkFrame :: [[a]] -> Frame
mkFrame l = Frame (length $ head l) (length l)

mkFramed :: (V.Vector v a) => [[a]] -> Framed v a
mkFramed l = Framed (mkFrame l) (V.fromList $ concat l)

mkFramedF :: (V.Vector v a) => (Vec2 Int -> a) -> Frame -> Framed v a
mkFramedF f frame = Framed frame . V.fromList $ f <$> frameCrds frame

framedThaw :: (V.Vector v a) => Framed v a -> ST s (Framed (V.Mutable v s) a)
framedThaw (Framed frame m) = Framed frame <$> V.thaw m

inFrame :: Frame -> Vec2 Int -> Bool
inFrame (Frame w h) (Vec2 x y) = x >= 0 && x < w && y >= 0 && y < h
{-# INLINE inFrame #-}

frameIdx :: Frame -> Vec2 Int -> Int
frameIdx (Frame w _) (Vec2 x y) = x + y * w
{-# INLINE frameIdx #-}

fromIdx :: Frame -> Int -> Vec2 Int
fromIdx (Frame w _) n = let (q, r) = n `divMod` w in Vec2 r q
{-# INLINE fromIdx #-}

-- |NOTE: Bound is not checked
getAt :: (V.Vector v a) => Framed v a -> Vec2 Int -> a
getAt (Framed frame m) p = m V.! frameIdx frame p
{-# INLINE getAt #-}

frameCrds :: Frame -> [Vec2 Int]
frameCrds (Frame w h) = [ Vec2 i j | j <- [0 .. pred h], i <- [0 .. pred w] ]

-- |Enumerated vector by a bounded enum type e, assuming minBound == 0.
newtype Enumed e v a = Enumed {
    underVec :: v a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

genEnumed :: (Enum e, Bounded e, V.Vector v a) => (e -> a) -> Enumed e v a
genEnumed f = Enumed $ V.generate (succ $ fromEnum maxBnd) (f . toEnum) where
  maxBnd = let m = maxBound; _ = f m in m

enumGet :: (Enum e, V.Vector v a) => Enumed e v a -> e -> a
enumGet (Enumed v) e = V.unsafeIndex v $ fromEnum e
{-# INLINE enumGet #-}

enumSets :: (Enum e, V.Vector v a) => [(e, a)] -> Enumed e v a -> Enumed e v a
enumSets xs (Enumed v) = Enumed $ V.unsafeUpd v [(fromEnum e, x) | (e, x) <- xs]
{-# INLINE enumSets #-}

enumModifies :: (Enum e, V.Vector v a) => (a -> b -> a) -> [(e, b)] -> Enumed e v a -> Enumed e v a
enumModifies f xs (Enumed v) = Enumed $ V.unsafeAccum f v [(fromEnum e, x) | (e, x) <- xs]
{-# INLINE enumModifies #-}