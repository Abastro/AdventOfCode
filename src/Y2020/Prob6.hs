module Y2020.Prob6 where

import Data.List (groupBy)
import Data.Char (ord)
import qualified Data.IntSet as S

nullSep :: [String] -> [[String]]
nullSep = filter (/= [""]) . groupBy (\x y -> null x == null y)

sol1 :: [String] -> Int
sol1 = sum . map (S.size . S.unions . map (S.fromList . map ord)) . nullSep

sol2 :: [String] -> Int
sol2 = sum . map (S.size . foldr1 S.intersection . map (S.fromList . map ord)) . nullSep

