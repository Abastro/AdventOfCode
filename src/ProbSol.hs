{-# LANGUAGE ExistentialQuantification #-}
module ProbSol
  ( ProbClass(..)
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
import           Text.ParserCombinators.ReadP   ( string )
import           Text.Printf
import           Text.Read

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
