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
