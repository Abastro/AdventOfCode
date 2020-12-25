module Main where

import qualified Data.Map as M

import qualified Y2020.Prob1
import qualified Y2020.Prob2
import qualified Y2020.Prob3
import qualified Y2020.Prob4
import qualified Y2020.Prob5

withRead :: String -> Int -> (String -> IO ()) -> (String, IO ())
withRead ident num app = (,) (ident <> "." <> show num) $ do
  file <- readFile $ "input/" <> ident
  app file

inLines :: (Read a, Show b) => ([a] -> b) -> String -> IO ()
inLines f = print . f . map read . lines

apps :: M.Map String (IO ())
apps = M.fromList [
    withRead "2020.01" 0 $ inLines $ Y2020.Prob1.sol1 2020
  , withRead "2020.01" 1 $ inLines $ Y2020.Prob1.sol2 2020
  , withRead "2020.02" 0 $ print . Y2020.Prob2.sol1 . lines
  , withRead "2020.02" 1 $ print . Y2020.Prob2.sol2 . lines
  , withRead "2020.03" 0 $ print . Y2020.Prob3.sol1 3 1 . lines
  , withRead "2020.03" 1 $ print . Y2020.Prob3.sol2 . lines
  , withRead "2020.04" 0 $ print . Y2020.Prob4.sol1
  , withRead "2020.04" 1 $ print . Y2020.Prob4.sol2
  , withRead "2020.05" 0 $ print . Y2020.Prob5.sol1 . lines
  , withRead "2020.05" 1 $ print . Y2020.Prob5.sol2 . lines
  ]

main :: IO ()
main = putStrLn "Put code:" >> getLine >>= (M.!) apps
