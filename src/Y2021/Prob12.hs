module Y2021.Prob12 ( sol12 ) where
import Common
import ProbSol
import Text.Read
import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntSet as IS

data Conn a = Conn !a !a deriving Foldable
instance Read (Conn String) where
  readPrec = Conn <$> readIdent <*> (do Symbol "-" <- lexP; readIdent) where
    readIdent = do Ident id <- lexP; pure id
mkGraph :: [Conn String] -> (Int, (V.Vector String, V.Vector [Int]))
mkGraph conns = (vToI M.! "start", (V.fromList allVs, graphs)) where
  allVs = S.toList . S.fromList $ conns >>= foldMap pure
  vToI = M.fromList $ zip allVs [0..]
  edges = [(vToI M.! v, vToI M.! w) | Conn v w <- conns] >>= (\(p, q) -> [(p, q), (q, p)])
  graphs = V.accum (flip (:)) (V.replicate (M.size vToI) []) edges

nPathFrom :: ProbClass -> (V.Vector String, V.Vector [Int]) -> (IS.IntSet, Maybe Int, Int) -> Int -- TODO Would memoization help?
nPathFrom cl (label, graph) (visited, twice, vert) = lastPath $ sum (pFrom <$> nexts) where 
  lastPath i = if label V.! vert == "end" then 1 else i -- Exit immediately after endpt
  visited' = if all isLower (label V.! vert) then IS.insert vert visited else visited
  twice' = twice <|> (vert <$ boolToMaybe (vert `IS.member` visited))
  canV Fst v = v `IS.notMember` visited'; canV Snd v = canV Fst v || (label V.! v /= "start" && isNothing twice')
  nexts = filter (canV cl) $ graph V.! vert
  pFrom w = nPathFrom cl (label, graph) (visited', twice', w)

sol12 :: ProbClass -> [String] -> Int
sol12 cl l = nPathFrom cl graph (IS.empty, Nothing, start) where
  (start, graph) = mkGraph $ read @(Conn String) <$> l