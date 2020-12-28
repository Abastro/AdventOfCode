module Y2020.Prob6 where

import Data.Char (ord)
import qualified Data.IntSet as S

import Common ( deintercalate )

sol1 :: [String] -> Int
sol1 = sum . map (S.size . S.unions . map (S.fromList . map ord)) . deintercalate []

sol2 :: [String] -> Int
sol2 = sum . map (S.size . foldr1 S.intersection . map (S.fromList . map ord)) . deintercalate []

