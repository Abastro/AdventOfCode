{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Y2021.Y2021
  ( yr
  , psols
  ) where
import qualified Data.Map                      as M
import           ProbSol

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
import           Y2021.Prob11
import           Y2021.Prob12
import           Y2021.Prob13
import           Y2021.Prob14
import           Y2021.Prob15
import           Y2021.Prob16
import           Y2021.Prob17
import           Y2021.Prob18
import           Y2021.Prob19
import           Y2021.Prob20
import           Y2021.Prob21
import           Y2021.Prob22
import           Y2021.Prob23
import           Y2021.Prob24
import           Y2021.Prob25

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
  , ofProb 11 Fst ifLines             1669          sol11F
  , ofProb 11 Snd ifLines             351           sol11S
  , ofProb 12 Fst ifLines             4885          (sol12 Fst)
  , ofProb 12 Snd ifLines             117095        (sol12 Snd)
  , ofProb 13 Fst ifLines             735           sol13F
  , ofProb
    13
    Snd
    ifLines
    (unlines
      [ "#..#.####.###..####.#..#..##..#..#.####"
      , "#..#.#....#..#....#.#.#..#..#.#..#....#"
      , "#..#.###..#..#...#..##...#..#.#..#...#."
      , "#..#.#....###...#...#.#..####.#..#..#.."
      , "#..#.#....#.#..#....#.#..#..#.#..#.#..."
      , ".##..#....#..#.####.#..#.#..#..##..####"
      ]
    )
    sol13S
  , ofProb 14 Fst ((10, ) <$> ifLines) 2584             sol14
  , ofProb 14 Snd ((40, ) <$> ifLines) 3816397135460    sol14
  , ofProb 15 Fst ifLines              652              sol15F
  , ofProb 15 Snd ifLines              2938             sol15S
  , ofProb 16 Fst ifStr                906              sol16F
  , ofProb 16 Snd ifStr                819324480368     sol16S
  , ofProb 17 Fst ifStr                10011            sol17F
  , ofProb 17 Snd ifStr                2994             sol17S
  , ofProb 18 Fst ifLines              3691             sol18F
  , ofProb 18 Snd ifLines              4756             sol18S
  , ofProb 19 Fst ifLines              434              sol19F
  , ofProb 19 Snd ifLines              11906            sol19S
  , ofProb 20 Fst ((2, ) <$> ifLines)  4968             sol20
  , ofProb 20 Snd ((50, ) <$> ifLines) 16793            sol20
  , ofProb 21 Fst ifLines              929625           sol21F
  , ofProb 21 Snd ifLines              175731756652760  sol21S
  , ofProb 22 Fst ifLines              615869           sol22F
  , ofProb 22 Snd ifLines              1323862415207825 sol22S
  , ofProb 23 Fst ifLines              14467            sol23F
  , ofProb 23 Snd ifLines              48759            sol23S
  , ofProb 24 Fst ifLines              51983999947999   sol24F
  , ofProb 24 Snd ifLines              11211791111365   sol24S
  , ofProb 25 Fst ifLines              598              sol25F
  ]
