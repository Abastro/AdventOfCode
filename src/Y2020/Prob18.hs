module Y2020.Prob18 ( solP18F, solP18S ) where

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

solP18F :: [String] -> Int
solP18F inp = sum $ evaluate . read <$> inp

solP18S :: [String] -> Int
solP18S inp = sum $ evaluate . getExpr . read <$> inp