{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Y2021.Y2021
  ( yr
  , psols
  ) where
import           Common
import qualified Data.Map                      as M

import           Y2021.Prob01
import           Y2021.Prob02
import           Y2021.Prob03

yr :: String
yr = "2021"

ofProb n cl i exp sol = (ProbN n cl, mkSeal (mkProb yr n i exp) (Sol sol))
ofProb2 n cl i exp sol sol2 =
  (ProbN n cl, Seal (mkProb yr n i exp) (Sol sol) [Sol sol2])

psols :: M.Map ProbN SealPS
psols = M.fromList
  [ ofProb 1 Fst ifNums  1374       sol1F
  , ofProb 1 Snd ifNums  1418       sol1S
  , ofProb 2 Fst ifLines 1660158    sol2F
  , ofProb 2 Snd ifLines 1604592846 sol2S
  , ofProb 3 Fst ifLines 1997414    sol3F
  , ofProb 3 Snd ifLines 0 sol3S
  ]
