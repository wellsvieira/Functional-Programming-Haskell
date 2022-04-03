data BTree a = Empty
             | Node a (BTree a) (BTree a)
        deriving Show

altura :: BTree a -> Int
altura Empty = 0
altura (Node n e d) = max (1+altura e) (1+altura d)

contaNodos :: BTree a -> Int
contaNodos Empty =0
contaNodos (Node n e d) = 1 + contaNodos e + contaNodos d

path :: [Bool] -> BTree a -> [a]
path l Empty = []
path [] _ = []
path (x:xs) (Node r e d) | x == False = r:path xs e
                         | otherwise = r:path xs d

mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror r) (mirror e)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r e d) (Node r2 e2 d2) = Node (f r r2) (zipWithBT f e e2) (zipWithBT f r r2)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
