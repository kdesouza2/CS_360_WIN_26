mysum :: Num a => [a] -> a
mysum [] = 0
mysum (n:ns) = n + mysum ns

mysum2 ns = foldr (+) 0 ns

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)
