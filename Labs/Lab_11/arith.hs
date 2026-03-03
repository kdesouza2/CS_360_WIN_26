type Assoc k v = [(k,v)]
type Env = Assoc Char Int
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data ArithExpr = Const Int
               | Var Char
               | Add ArithExpr ArithExpr
               | Mult ArithExpr ArithExpr
               deriving Show

eval :: ArithExpr -> Env -> Int
eval (Const a) _ = a
eval (Var x) t = find x t
eval (Add e1 e2) t = (eval e1 t) + (eval e2 t)
eval (Mult e1 e2) t = (eval e1 t) * (eval e2 t)

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


