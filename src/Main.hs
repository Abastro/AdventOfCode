{-# LANGUAGE Rank2Types #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           Data.Foldable
import qualified Data.Map                      as M
import           ProbSol
import           System.IO
import           System.TimeIt                  ( timeIt )
import           Text.Printf
import           Text.Read

import qualified Y2020.Y2020                   as Y2020
import qualified Y2021.Y2021                   as Y2021

apps :: M.Map String (M.Map ProbN SealPS)
apps = M.fromList [(Y2020.yr, Y2020.psols), (Y2021.yr, Y2021.psols)]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  yr <- putStr "Year: " >> getLine
  maybe (putStrLn "Unknown Year") onYear (apps M.!? yr)
 where
  onYear psols = do
    putStr "Code: " >> getLine >>= \case
      "bench" ->
        defaultMain [bgroup "AoC" $ uncurry benchPS <$> M.toList psols]
      "all" -> for_ (M.toList psols) $ \(pn, Seal prob sol _) -> do
        putStrLn "" >> putStrLn (show pn <> ":")
        onPS prob sol
      pid ->
        maybe (putStrLn "Wrong code") onSeal (readMaybe pid >>= (psols M.!?))

  onSeal :: SealPS -> IO ()
  onSeal (Seal prob sol others) = do
    onPS prob sol
    unless (null others) $ do
      ans <- putStr "Enter Y to try another: " >> getLine
      when (ans == "Y") $ traverse_ (onPS prob) others

  onPS prob sol = do
    let InpForm form = inpForm prob
    file <- readFile (inpLoc prob)
    res  <- timeIt $ pure $! solution sol (form file)
    let exp = expAns prob
    if exp == res
      then printf "Matches: %s\n" (show res)
      else printf "Expected: %s, Actual: %s\n" (show exp) (show res)

  benchPS pn (Seal prob sol _) = bench (show pn) $ nfIO $ do
    let InpForm form = inpForm prob
    file <- readFile (inpLoc prob)
    let exp = expAns prob
    let res = solution sol $ form file
    pure (exp == res)
