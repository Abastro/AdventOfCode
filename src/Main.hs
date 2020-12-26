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

withRead :: String -> Int -> (String -> a) -> (String, IO a)
withRead ident num app = (,) (ident <> "." <> show num) $ do
  file <- readFile $ "input/" <> ident
  pure $ app file

expecting :: (Eq a, Show a) => a -> a -> String
expecting exp got = if exp == got then "Matches, Actual:" <> show got
  else "Expected: " <> show exp <> ", Actual: " <> show got

-- Apparently 4, 7, 10, 13, 18-20, 23 are hard ones but does not feel as such
apps :: M.Map String (IO String)
apps = M.fromList [
    withRead "2020.01" 0 $ expecting 786811 . Y2020.Prob1.sol1 2020 . map read . lines
  , withRead "2020.01" 1 $ expecting 199068980 . Y2020.Prob1.sol2 2020 . map read . lines
  , withRead "2020.02" 0 $ expecting 445 . Y2020.Prob2.sol1 . lines
  , withRead "2020.02" 1 $ expecting 491 . Y2020.Prob2.sol2 . lines
  , withRead "2020.03" 0 $ expecting 276 . Y2020.Prob3.sol1 3 1 . lines
  , withRead "2020.03" 1 $ expecting 7812180000 . Y2020.Prob3.sol2 . lines
  , withRead "2020.04" 0 $ expecting 247 . Y2020.Prob4.sol1
  , withRead "2020.04" 1 $ expecting 145 . Y2020.Prob4.sol2
  , withRead "2020.05" 0 $ expecting 998 . Y2020.Prob5.sol1 . lines
  , withRead "2020.05" 1 $ expecting 676 . Y2020.Prob5.sol2 . lines
  , withRead "2020.06" 0 $ expecting 6633 . Y2020.Prob6.sol1 . lines
  , withRead "2020.06" 1 $ expecting 3202 . Y2020.Prob6.sol2 . lines
  , withRead "2020.07" 0 $ expecting 235 . Y2020.Prob7.sol1 . lines
  , withRead "2020.07" 1 $ expecting 158493 . Y2020.Prob7.sol2 . lines
  , withRead "2020.08" 0 $ expecting 1200 . Y2020.Prob8.sol1 . lines
  , withRead "2020.08" 1 $ expecting 1023 . Y2020.Prob8.sol2 . lines
  , withRead "2020.09" 0 $ expecting 530627549 . Y2020.Prob9.sol1 . map read . lines
  , withRead "2020.09" 1 $ expecting 77730285 . Y2020.Prob9.sol2 . map read . lines
  , withRead "2020.10" 0 $ expecting 1700 . Y2020.Prob10.sol1 . map read . lines
  , withRead "2020.10" 1 $ expecting 12401793332096 . Y2020.Prob10.sol2 . map read . lines
  , withRead "2020.11" 0 $ show . Y2020.Prob11.sol1 . lines
  ]

runAll :: IO String
runAll = unlines . map show . M.toList <$> sequenceA apps

main :: IO ()
main = putStrLn "Put code:" >> getLine
  >>= (M.!) (M.insert "all" runAll apps) >>= putStrLn
