module Y2020.Y2020
  ( yr
  , psols
  ) where
import qualified Data.Map                      as M
import           ProbSol

import           Y2020.Prob1
import           Y2020.Prob10
import           Y2020.Prob11
import           Y2020.Prob12
import           Y2020.Prob13
import           Y2020.Prob14
import           Y2020.Prob15
import           Y2020.Prob15a
import           Y2020.Prob16
import           Y2020.Prob17
import           Y2020.Prob18
import           Y2020.Prob19
import           Y2020.Prob2
import           Y2020.Prob20
import           Y2020.Prob21
import           Y2020.Prob22
import           Y2020.Prob23
import           Y2020.Prob23a
import           Y2020.Prob24
import           Y2020.Prob25
import           Y2020.Prob3
import           Y2020.Prob4
import           Y2020.Prob5
import           Y2020.Prob6
import           Y2020.Prob7
import           Y2020.Prob8
import           Y2020.Prob9

yr :: String
yr = "2020"

ofProb n cl i exp sol = (ProbN n cl, mkSeal (mkProb yr n i exp) (Sol sol))
ofProb2 n cl i exp sol sol2 =
  (ProbN n cl, Seal (mkProb yr n i exp) (Sol sol) [Sol sol2])

psols :: M.Map ProbN SealPS
psols = M.fromList
  [ ofProb 1  Fst ((2, 2020, ) <$> ifNums) 786811          solP1
  , ofProb 1  Snd ((3, 2020, ) <$> ifNums) 199068980       solP1
  , ofProb 2  Fst ifLines                  445             solP2F
  , ofProb 2  Snd ifLines                  491             solP2S
  , ofProb 3  Fst ifLines                  276             solP3F
  , ofProb 3  Snd ifLines                  7812180000      solP3S
  , ofProb 4  Fst ifLines                  247             solP4F
  , ofProb 4  Snd ifLines                  145             solP4S
  , ofProb 5  Fst ifLines                  998             solP5F
  , ofProb 5  Snd ifLines                  676             solP5S
  , ofProb 6  Fst ifLines                  6633            solP6F
  , ofProb 6  Snd ifLines                  3202            solP6S
  , ofProb 7  Fst ifLines                  235             solP7F
  , ofProb 7  Snd ifLines                  158493          solP7S
  , ofProb 8  Fst ifLines                  1200            solP8F
  , ofProb 8  Snd ifLines                  1023            solP8S
  , ofProb 9  Fst ifNums                   530627549       solP9F
  , ofProb 9  Snd ifNums                   77730285        solP9S
  , ofProb 10 Fst ifNums                   1700            solP10F
  , ofProb 10 Snd ifNums                   12401793332096  solP10S
  , ofProb 11 Fst ifLines                  2166            solP11F
  , ofProb 11 Snd ifLines                  1955            solP11S
  , ofProb 12 Fst ifLines                  2879            solP12F
  , ofProb 12 Snd ifLines                  178986          solP12S
  , ofProb 13 Fst ifLines                  2382            solP13F
  , ofProb 13 Snd ifLines                  906332393333683 solP13S
  , ofProb 14 Fst ifLines                  9879607673316   solP14F
  , ofProb 14 Snd ifLines                  3435342392262   solP14S
  , ofProb2 15 Fst ((2020, ) <$> ifStr)     706   solP15a solP15
  , ofProb2 15 Snd ((30000000, ) <$> ifStr) 19331 solP15a solP15
  , ofProb 16 Fst ifLines             24110           solP16F
  , ofProb 16 Snd ifLines             6766503490793   solP16S
  , ofProb 17 Fst ((3, ) <$> ifLines) 424             solP17
  , ofProb 17 Snd ((4, ) <$> ifLines) 2460            solP17
  , ofProb 18 Fst ifLines             6640667297513   solP18F
  , ofProb 18 Snd ifLines             451589894841552 solP18S
  , ofProb 19 Fst ifLines             122             solP19F
  , ofProb 19 Snd ifLines             287             solP19S
  , ofProb 20 Fst ifLines             59187348943703  solP20F
  , ofProb 20 Snd ifLines             1565            solP20S
  , ofProb 21 Fst ifLines             2211            solP21F
  , ofProb 21
           Snd
           ifLines
           "vv,nlxsmb,rnbhjk,bvnkk,ttxvphb,qmkz,trmzkcfg,jpvz"
           solP21S
  , ofProb 22 Fst ifLines 32677 solP22F
  , ofProb 22 Snd ifLines 33661 solP22S
  , ofProb2 23 Fst ifStr "69852437"  solP23Fa solP23F
  , ofProb2 23 Snd ifStr 91408386135 solP23Sa solP23S
  , ofProb 24 Fst ifLines 326      solP24F
  , ofProb 24 Snd ifLines 3979     solP24S
  , ofProb 25 Fst ifNums  19414467 solP25
  ]

