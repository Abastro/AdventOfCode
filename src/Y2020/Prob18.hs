module Y2020.Prob18 ( sol1, sol2 ) where

import Text.Read ( Read(..), Lexeme(..), prec, step, parens, choice, lexP )

data Expr = Num Int | Expr :+: Expr | Expr :*: Expr deriving Show
instance Read Expr where
  readPrec = parens $ choice [ prec 1 $ do p <- readPrec; ($ Num p) <$> readRest
    , prec 0 $ do e <- step readPrec; ($ e) <$> readRest
    ] where
      readRest = choice [ pure id
        , prec 0 $ do Symbol "+" <- lexP; e' <- step readPrec
                      re <- readRest; pure $ \e -> re (e :+: e')
        , prec 0 $ do Symbol "*" <- lexP; e' <- step readPrec
                      re <- readRest; pure $ \e -> re (e :*: e') ]
newtype Expr' = Expr' { getExpr :: Expr }
instance Read Expr' where
  readPrec = fmap Expr' $ parens $ choice [prec 2 $ Num <$> readPrec
    , prec 1 $ do e <- step readPrec; Symbol "+" <- lexP; e' <- readPrec
                  pure $ getExpr e :+: getExpr e'
    , prec 0 $ do e <- step readPrec; Symbol "*" <- lexP; e' <- readPrec
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