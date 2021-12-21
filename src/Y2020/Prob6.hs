module Y2020.Prob6 ( solP6F, solP6S ) where

import Data.Char ( ord )
import qualified Data.IntSet as S
import Common ( deintercalate )

solP6F :: [String] -> Int
solP6F = sum . map (S.size . S.unions . map (S.fromList . map ord)) . deintercalate []

solP6S :: [String] -> Int
solP6S = sum . map (S.size . foldr1 S.intersection . map (S.fromList . map ord)) . deintercalate []

