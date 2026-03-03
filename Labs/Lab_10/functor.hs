{-
class Functor f where
   fmap :: (a->b)-> f a -> f b

instance Functor Maybe where
   -- fmap :: (a -> b )-> Maybe a -> Maybe b
   fmap _ Nothing = Nothing
   fmap g (Just x) = Just (g x)

-}
 
data Tree a = Leaf a | Node (Tree a) (Tree a)
   deriving Show

instance Functor Tree where
   -- fmap :: (a -> b)-> Tree a -> Tree b
   fmap g (Leaf x)   = Leaf (g x)
   fmap g (Node l r) = Node (fmap g l) (fmap g r)

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

{-
inc (Just 1)
inc [1..5]
t = Node (Leaf 1) (Leaf 2)
inc t
-}
