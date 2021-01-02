module Main where

import qualified Data.Map as M

import qualified Y2020.Prob1
import qualified Y2020.Prob2
import qualified Y2020.Prob3
import qualified Y2020.Prob4
import qualified Y2020.Prob5
import qualified Y2020.Prob6
import qualified Y2020.Prob7
import qualified Y2020.Prob8
import qualified Y2020.Prob9
import qualified Y2020.Prob10
import qualified Y2020.Prob11
import qualified Y2020.Prob12
import qualified Y2020.Prob13
import qualified Y2020.Prob14
import qualified Y2020.Prob15
import qualified Y2020.Prob16
import qualified Y2020.Prob17
import qualified Y2020.Prob18
import qualified Y2020.Prob19
import qualified Y2020.Prob20
import qualified Y2020.Prob21
import qualified Y2020.Prob22
import qualified Y2020.Prob23
import qualified Y2020.Prob24
import qualified Y2020.Prob25

withRead :: String -> Int -> (String -> a) -> (String, IO a)
withRead ident num app = (,) (ident <> "." <> show num) $ do
  file <- readFile $ "input/" <> ident
  pure $ app file

expect :: (Eq a, Show a) => a -> a -> String
expect exp got = if exp == got then "Matches, Actual:" <> show got
  else "Expected: " <> show exp <> ", Actual: " <> show got

apps :: M.Map String (IO String)
apps = M.fromList [
    withRead "2020.01" 0 $ expect 786811 . Y2020.Prob1.sol 2 2020 . map read . lines
  , withRead "2020.01" 1 $ expect 199068980 . Y2020.Prob1.sol 3 2020 . map read . lines
  , withRead "2020.02" 0 $ expect 445 . Y2020.Prob2.sol1 . lines
  , withRead "2020.02" 1 $ expect 491 . Y2020.Prob2.sol2 . lines
  , withRead "2020.03" 0 $ expect 276 . Y2020.Prob3.sol1 3 1 . lines
  , withRead "2020.03" 1 $ expect 7812180000 . Y2020.Prob3.sol2 . lines
  , withRead "2020.04" 0 $ expect 247 . Y2020.Prob4.sol1 . lines
  , withRead "2020.04" 1 $ expect 145 . Y2020.Prob4.sol2 . lines
  , withRead "2020.05" 0 $ expect 998 . Y2020.Prob5.sol1 . lines
  , withRead "2020.05" 1 $ expect 676 . Y2020.Prob5.sol2 . lines
  , withRead "2020.06" 0 $ expect 6633 . Y2020.Prob6.sol1 . lines
  , withRead "2020.06" 1 $ expect 3202 . Y2020.Prob6.sol2 . lines
  , withRead "2020.07" 0 $ expect 235 . Y2020.Prob7.sol1 . lines
  , withRead "2020.07" 1 $ expect 158493 . Y2020.Prob7.sol2 . lines
  , withRead "2020.08" 0 $ expect 1200 . Y2020.Prob8.sol1 . lines
  , withRead "2020.08" 1 $ expect 1023 . Y2020.Prob8.sol2 . lines
  , withRead "2020.09" 0 $ expect 530627549 . Y2020.Prob9.sol1 . map read . lines
  , withRead "2020.09" 1 $ expect 77730285 . Y2020.Prob9.sol2 . map read . lines
  , withRead "2020.10" 0 $ expect 1700 . Y2020.Prob10.sol1 . map read . lines
  , withRead "2020.10" 1 $ expect 12401793332096 . Y2020.Prob10.sol2 . map read . lines
  , withRead "2020.11" 0 $ expect 2166 . Y2020.Prob11.sol1 . lines
  , withRead "2020.11" 1 $ expect 1955 . Y2020.Prob11.sol2 . lines
  , withRead "2020.12" 0 $ expect 2879. Y2020.Prob12.sol1 . lines
  , withRead "2020.12" 1 $ expect 178986 . Y2020.Prob12.sol2 . lines
  , withRead "2020.13" 0 $ expect 2382 . Y2020.Prob13.sol1 . lines
  , withRead "2020.13" 1 $ expect 906332393333683 . Y2020.Prob13.sol2 . lines
  , withRead "2020.14" 0 $ expect 9879607673316 . Y2020.Prob14.sol1 . lines
  , withRead "2020.14" 1 $ expect 3435342392262 . Y2020.Prob14.sol2 . lines
  , withRead "2020.15" 0 $ expect 706 . Y2020.Prob15.sol 2020
  , withRead "2020.15" 1 $ expect 19331 . Y2020.Prob15.sol 30000000
  , withRead "2020.16" 0 $ expect 24110 . Y2020.Prob16.sol1 . lines
  , withRead "2020.16" 1 $ expect 6766503490793 . Y2020.Prob16.sol2 . lines
  , withRead "2020.17" 0 $ expect 424 . Y2020.Prob17.sol1 . lines
  , withRead "2020.17" 1 $ expect 2460 . Y2020.Prob17.sol2 . lines
  , withRead "2020.18" 0 $ expect 6640667297513 . Y2020.Prob18.sol1 . lines
  , withRead "2020.18" 1 $ expect 451589894841552 . Y2020.Prob18.sol2 . lines
  , withRead "2020.19" 0 $ expect 122 . Y2020.Prob19.sol1 . lines
  , withRead "2020.19" 1 $ expect 287 . Y2020.Prob19.sol2 . lines
  , withRead "2020.20" 0 $ expect 59187348943703 . Y2020.Prob20.sol1 . lines
  , withRead "2020.20" 1 $ expect 1565 . Y2020.Prob20.sol2 . lines
  , withRead "2020.21" 0 $ expect 2211 . Y2020.Prob21.sol1 . lines
  , withRead "2020.21" 1 $ expect "vv,nlxsmb,rnbhjk,bvnkk,ttxvphb,qmkz,trmzkcfg,jpvz"
    . Y2020.Prob21.sol2 . lines
  , withRead "2020.22" 0 $ expect 32677 . Y2020.Prob22.sol1 . lines
  , withRead "2020.22" 1 $ expect 33661 . Y2020.Prob22.sol2 . lines
  , withRead "2020.23" 0 $ expect "69852437" . Y2020.Prob23.sol1
  , withRead "2020.23" 1 $ expect 91408386135 . Y2020.Prob23.sol2
  , withRead "2020.24" 0 $ expect 326 . Y2020.Prob24.sol1 . lines
  , withRead "2020.24" 1 $ expect 3979 . Y2020.Prob24.sol2 . lines
  , withRead "2020.25" 0 $ expect 19414467 . Y2020.Prob25.sol . map read . lines
  ]

runAll :: IO String
runAll = unlines . map show . M.toList <$> sequenceA (foldr M.delete apps [
  "2020.15.1", "2020.23.1"])

main :: IO ()
main = putStrLn "Put code:" >> getLine
  >>= sequenceA . (M.!?) (M.insert "all" runAll apps)
  >>= maybe (putStrLn "Wrong code") putStrLn
