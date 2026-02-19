{- - (define (fact n)
      (if (= n 0)
          1
          (* n (fact (- n 1)))))
 -}


fact1 :: Integer -> Integer
-- conditional

fact1 n = if n == 0 then 1 else n * fact1 (n - 1)

fact2 :: Integer -> Integer
-- guarded equations

fact2 n | n == 1  = 1
        | otherwise = n * fact2 (n - 1)

fact3 :: Integer -> Integer
-- pattern matching

fact3 0 = 1
fact3 n = n * fact3 (n - 1)

fact4 n = product [1..n]

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

fact5 n = foldr (*) 1 [1..n]

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v []     = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)
