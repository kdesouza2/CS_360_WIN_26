data Expr = Val Int | Div Expr Expr

eval0 :: Expr -> Int
eval0 (Val x) = x
eval0 (Div x y) = (eval0 x) `div` (eval0 y)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval1 :: Expr -> Maybe Int
eval1 (Val x) = Just x
eval1 (Div x y) = case eval1 x of
                       Nothing -> Nothing
                       Just n -> case eval1 y of
                                      Nothing -> Nothing
                                      Just m -> safediv n m

{-
(>>=) :: Maybe a -> (a -> Maybe) -> Maybe b
mx >>= f = case mx of
                Nothing -> Nothing
                Just x  -> f x

-}

eval2 :: Expr -> Maybe Int
eval2 (Val x)   = Just x
eval2 (Div x y) = eval2 x >>= \n ->
                  eval2 y >>= \m ->
                  safediv n m

eval3 :: Expr -> Maybe Int
eval3 (Val x)   = Just x
eval3 (Div x y) = do n <- eval3 x
                     m <- eval3 y
                     safediv n m

