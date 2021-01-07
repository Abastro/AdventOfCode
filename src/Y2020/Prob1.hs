module Y2020.Prob1 ( sol ) where

import Control.Monad ( replicateM )
import qualified Data.IntSet as S

sol :: Int -> Int -> [Int] -> Int
sol n tar list = product . head $ filter ((`S.member` S.fromList list) . head)
  $ (\l -> tar - sum l : l) <$> replicateM (n-1) list