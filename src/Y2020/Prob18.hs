module Y2020.Prob18 ( sol1, sol2 ) where

import Text.Read ( Read(..), prec, step, parens, choice, lexP, lift )
import Text.Read.Lex ( Lexeme(..), expect )
import Common ( numToInt )

data Expr = Num Int | Expr :+: Expr | Expr :*: Expr deriving Show
instance Read Expr where
  readPrec = parens $ choice [prec 1 $ do
      Number p <- lexP
      ($ Num (numToInt p)) <$> readRest
    , prec 0 $ do
      e <- step readPrec
      ($ e) <$> readRest
    ] where
      readRest = choice [pure id, prec 0 $ do
          lift . expect $ Symbol "+"; e' <- step readPrec
          re <- readRest; pure $ \e -> re (e :+: e')
        , prec 0 $ do
          lift . expect $ Symbol "*"; e' <- step readPrec
          re <- readRest; pure $ \e -> re (e :*: e')
        ]
newtype Expr' = Expr' { getExpr :: Expr }
instance Read Expr' where
  readPrec = fmap Expr' $ parens $ choice [prec 2 $ do
      Number p <- lexP; pure $ Num $ numToInt p
    , prec 1 $ do
      e <- step readPrec; lift . expect $ Symbol "+"; e' <- readPrec
      pure $ getExpr e :+: getExpr e'
    , prec 0 $ do
      e <- step readPrec; lift . expect $ Symbol "*"; e' <- readPrec
      pure $ getExpr e :*: getExpr e'
    ]

evaluate :: Expr -> Int
evaluate (Num n) = n
evaluate (e :+: e') = evaluate e + evaluate e'
evaluate (e :*: e') = evaluate e * evaluate e'

sol1 :: [String] -> Int
sol1 inp = sum $ evaluate . read <$> inp

sol2 :: [String] -> Int
sol2 inp = sum $ evaluate . getExpr . read <$> inp
