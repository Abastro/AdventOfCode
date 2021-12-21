module Y2020.Prob1 ( solP1 ) where

import Control.Monad ( replicateM )
import qualified Data.IntSet as S

solP1 :: (Int, Int, [Int]) -> Int
solP1 (n, tar, list) = product . head $ filter ((`S.member` S.fromList list) . head)
  $ (\l -> tar - sum l : l) <$> replicateM (n-1) list