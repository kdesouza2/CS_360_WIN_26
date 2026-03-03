safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

type Assoc k v = [(k,v)]
type Env = Assoc Char Int
find :: Eq k => k -> Assoc k v -> Maybe v
find k t = safehead [v | (k',v) <- t, k == k']

data ArithExpr = Const Int
               | Var Char
               | Add ArithExpr ArithExpr
               | Mult ArithExpr ArithExpr
               deriving Show

meval :: ArithExpr -> Env -> Maybe Int
meval (Const n) _    = return n
meval (Var x) t      = find x t
meval (Add e1 e2) t  = do m <- meval e1 t
                          n <- meval e2 t
                          return (m + n)
meval (Mult e1 e2) t = do m <- meval e1 t
                          n <- meval e2 t
                          return (m * n)

env :: Env
env = [('a',2),('b',3)]

t1 :: ArithExpr
-- Just 2
t1 = (Var 'a')

t2 :: ArithExpr
-- Nothing
t2 = (Var 'c')

t3 :: ArithExpr
-- Just 4
t3 = (Add (Var 'a') (Const 2))

t4 :: ArithExpr
-- Nothing
t4 = (Add (Var 'c') (Const 2))

