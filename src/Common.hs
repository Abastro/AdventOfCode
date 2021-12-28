{-# LANGUAGE ExistentialQuantification #-}
module Common
  ( c2word
  , inRange
  , applyN
  , boolToMaybe
  , count
  , deintercalate
  , liftFn
  , ProbClass(..)
  , ProbN(..)
  , InputForm(..)
  , ifStr
  , ifLines
  , ifNums
  , Problem(..)
  , mkProb
  , Solution(..)
  , SealPS(..)
  , mkSeal
  ) where

import           Data.Char                      ( ord )
import           Data.Function                  ( on )
import           Data.List                      ( groupBy )
import           Data.Word                      ( Word8 )
import           GHC.Read
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , string
                                                )
import           Text.ParserCombinators.ReadPrec
import           Text.Printf

-- Unsafe stuff
c2word :: Char -> Word8
c2word = fromIntegral . ord

inRange :: (Int, Int) -> Int -> Bool
inRange (lb, ub) = (&&) <$> (>= lb) <*> (< ub)

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = go n x where
  go 0 x = x
  go n x = let x' = f x in x' `seq` go (pred n) x'

boolToMaybe :: Bool -> Maybe ()
boolToMaybe f = if f then Just () else Nothing

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

deintercalate :: Eq a => a -> [a] -> [[a]]
deintercalate p = filter (/= [p]) . groupBy ((==) `on` (== p))

liftFn :: (ReadP a -> ReadP b) -> (ReadPrec a -> ReadPrec b)
liftFn f = readP_to_Prec . (f .) . readPrec_to_P

-- TODO EnumMaps, 2D/3D maps


-- TODO Isolate the problem class
data ProbClass = Fst | Snd
  deriving (Eq, Ord, Show, Read)
data ProbN = ProbN !Int !ProbClass
  deriving (Eq, Ord)
instance Show ProbN where
  show (ProbN n cl) = show n <> "." <> show cl
instance Read ProbN where
  readPrec = ProbN <$> readPrec <*> (lift (string ".") *> readPrec)

data Problem r a = Prob
  { inpLoc  :: String
  , inpForm :: InputForm r
  , expAns  :: a
  }
mkProb :: String -> Int -> InputForm r -> a -> Problem r a
mkProb yr n = Prob (printf "input/%s/%02d" yr n)

newtype Solution r a = Sol
  { solution :: r -> a }
data SealPS = forall r a . (Eq a, Show a) => Seal
  { theProb  :: Problem r a
  -- |Primary solution to be listed
  , theSol   :: Solution r a
  -- |Suboptimal solutions
  , otherSol :: [Solution r a]
  }
mkSeal :: (Eq a, Show a) => Problem r a -> Solution r a -> SealPS
mkSeal p s = Seal p s []

newtype InputForm r = InpForm (String -> r)
  deriving Functor
ifStr :: InputForm String
ifStr = InpForm id
ifLines :: InputForm [String]
ifLines = InpForm lines
ifNums :: InputForm [Int]
ifNums = InpForm (map read . lines)

