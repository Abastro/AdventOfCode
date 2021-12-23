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
import           Y2021.Prob04
import           Y2021.Prob05
import           Y2021.Prob06
import           Y2021.Prob07
import           Y2021.Prob08
import           Y2021.Prob09
import           Y2021.Prob10

yr :: String
yr = "2021"

ofProb n cl i exp sol = (ProbN n cl, mkSeal (mkProb yr n i exp) (Sol sol))
ofProb2 n cl i exp sol sol2 =
  (ProbN n cl, Seal (mkProb yr n i exp) (Sol sol) [Sol sol2])

psols :: M.Map ProbN SealPS
psols = M.fromList
  [ ofProb 1  Fst ifNums              1374          sol1F
  , ofProb 1  Snd ifNums              1418          sol1S
  , ofProb 2  Fst ifLines             1660158       sol2F
  , ofProb 2  Snd ifLines             1604592846    sol2S
  , ofProb 3  Fst ifLines             1997414       sol3F
  , ofProb 3  Snd ifLines             1032597       sol3S
  , ofProb 4  Fst ifLines             8580          sol4F
  , ofProb 4  Snd ifLines             9576          sol4S
  , ofProb 5  Fst ifLines             5585          sol5F
  , ofProb 5  Snd ifLines             17193         sol5S
  , ofProb 6  Fst ((, 80) <$> ifStr)  390923        sol6
  , ofProb 6  Snd ((, 256) <$> ifStr) 1749945484935 sol6
  , ofProb 7  Fst ifStr               356992        sol7F
  , ofProb 7  Snd ifStr               101268110     sol7S
  , ofProb 8  Fst ifLines             301           sol8F
  , ofProb 8  Snd ifLines             908067        sol8S
  , ofProb 9  Fst ifLines             631           sol9F
  , ofProb 9  Snd ifLines             821560        sol9S
  , ofProb 10 Fst ifLines             366027        sol10F
  , ofProb 10 Snd ifLines             1118645287    sol10S
  ]
