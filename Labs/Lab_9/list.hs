{-
 (define (append L1 L2)
  (if (null? L1)
      L2
      (cons (first L1) (append (rest L1) L2))))

(define (map f L)
  (if (null? L)
      null
      (cons (f (first L))
            (map f (rest L)))))

(define (reduce f init L)
  (if (null? L)
      init
      (f (first L) (reduce f init (rest L)))))
-}

-- works for lists of any type
-- parametric polymorphism

myappend :: [a] -> [a] -> [a]
myappend [] ys = ys
myappend (x:xs) ys = x : myappend xs ys

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

reduce :: (a -> b -> b) -> b -> [a] -> b
reduce f s [] = s
reduce f s (x:xs) = f x (reduce f s xs)

data List a = Nil | Cons a (List a)
     deriving Show

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

