module Y2020.Prob1 ( sol ) where

import Control.Monad ( replicateM )
import Data.List ( find )
import Data.Maybe ( fromJust )
import qualified Data.IntSet as S

sol :: Int -> Int -> [Int] -> Int
sol n tar list = product . fromJust
  $ find (flip S.member (S.fromList list) . head)
  $ (\l -> tar - sum l : l) <$> replicateM (n-1) list