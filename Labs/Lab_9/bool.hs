type Assoc k v = [(k,v)]
type Env = Assoc Char Bool
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

data BoolExpr = Const Bool
               | Var Char
               | Not BoolExpr 
               | And BoolExpr BoolExpr
               | Imply BoolExpr BoolExpr
               | Or  BoolExpr BoolExpr
               | Iff BoolExpr BoolExpr
     deriving Show

eval :: Env -> BoolExpr -> Bool
eval _ (Const a)    = a
eval t (Var x)      = find x t
eval t (Not e1)     = not (eval t e1)
eval t (And e1 e2)  = (eval t e1) && (eval t e2)
eval t (Imply e1 e2)  = (eval t e1) <= (eval t e2)
eval t (Or e1 e2)   = eval t e1 || eval t e2
eval t (Iff e1 e2)  = eval t e1 == eval t e2

vars :: BoolExpr -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q 
vars (Iff p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++  map (True:) bss
      where bss = bools (n - 1)

substs :: BoolExpr -> [Env]
substs e = map (zip vs) (bools (length vs))
      where vs = rmdups (vars e)

isTaut :: BoolExpr -> Bool
isTaut e = and [eval t e | t <- substs e ]

env :: Env
env = [('P',True),('Q',False)]

p1 :: BoolExpr
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: BoolExpr
p2 = Or (Var 'A') (Not (Var 'A'))

p3 :: BoolExpr
p3 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p4 :: BoolExpr
p4 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
