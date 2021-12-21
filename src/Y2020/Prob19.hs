module Y2020.Prob19 ( solP19F, solP19S ) where

import Data.List ( isPrefixOf )
import qualified Data.IntMap as M
import Text.Read ( Read(..), prec, step, choice, lift )
import Text.Read.Lex ( Lexeme(..), expect )
import qualified Text.ParserCombinators.ReadP as RP
import Common ( deintercalate )

data Rule = Str String | Refer Int | Rule ::: Rule | Rule :|: Rule
instance Read Rule where
  readPrec = choice [ prec 2 $ Str <$> readPrec
    , prec 2 $ Refer <$> readPrec
    , prec 1 $ do r <- step readPrec; r' <- readPrec; pure $ r ::: r'
    , prec 0 $ do r <- step readPrec; lift . expect $ Punc "|"; r' <- readPrec
                  pure $ r :|: r' ]

readInput :: [String] -> (M.IntMap Rule, [String])
readInput inp = let [rules, messages] = deintercalate [] inp in
  (, messages) . M.fromList $ do
    [rid, rule] <- deintercalate ':' <$> rules; [(read rid, read rule)]

withRule :: M.IntMap Rule -> Rule -> RP.ReadP ()
withRule _ (Str str) = RP.string str >> pure ()
withRule rules (Refer i) = withRule rules (rules M.! i)
withRule rules (r ::: r') = withRule rules r >> withRule rules r'
withRule rules (r :|: r') = withRule rules r RP.+++ withRule rules r'

solP19F :: [String] -> Int
solP19F inp = let (rules, inps) = readInput inp in length $ do
  (a, "") <- inps >>= RP.readP_to_S (withRule rules (rules M.! 0)); pure a

solP19S :: [String] -> Int
solP19S inp = solP19F $ ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]
  <> filter (\l -> not $ isPrefixOf "8:" l || isPrefixOf "11:" l) inp