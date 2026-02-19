data BinTree a = Null | Node a (BinTree a) (BinTree a)
                 deriving Show

-- example function on BinTree
numNodes :: BinTree a -> Int
numNodes Null       = 0
numNodes (Node v l r) = 1 + (numNodes l) + (numNodes r)

-- Sample BinTree
tree :: BinTree Int
tree = Node 3 (Node 1 Null Null)
               (Node 5 (Node 4 Null Null)
                          (Node 6 Null Null))

binSearch :: Ord a => a -> BinTree a -> Bool
binSearch x Null                     = False
binSearch x (Node y l r) | x == y    = True
                         | x < y     = binSearch x l
                         | otherwise = binSearch x r

