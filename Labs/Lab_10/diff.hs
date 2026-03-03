type Assoc k v = [(k,v)]
type Env = Assoc Char Int
type Var = Char
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

diff :: ArithExpr -> Var -> ArithExpr
diff (Const _) _    = (Const 0)
diff (Var v) x    
  | v == x          = (Const 1)
  | otherwise       = (Const 0)
diff (Add e1 e2) x  = (Add (diff e1 x) (diff e2 x))
diff (Mult e1 e2) x = (Add (Mult e1 (diff e2 x))
                           (Mult (diff e1 x) e2))
env :: Env
env = [('x',2),('y',3)]

e0 = (Const 5)
e1 = (Var 'x')
e2 = (Mult (Var 'x') (Add (Var 'x') (Const 5)))
