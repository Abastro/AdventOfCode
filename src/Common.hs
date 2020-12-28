module Common where

import Data.List ( groupBy )

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

deintercalate :: Eq a => a -> [a] -> [[a]]
deintercalate p = filter (/= [p]) . groupBy (\x y -> (x == p) == (y == p) )
