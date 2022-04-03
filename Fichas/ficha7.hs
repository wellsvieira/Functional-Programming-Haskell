--1----------

data ExpInt = Const Int 
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt 

--A
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = -(calcula x) 
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

--B
infixa :: ExpInt -> String 
infixa (Const x) = show x
infixa (Simetrico x) = "(-" ++ (infixa x) ++ ")"
infixa (Mais x y) = "(" ++ (infixa x) ++ " + " ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ " - " ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ " * " ++ (infixa y) ++ ")"

--C
posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = (posfixa x) ++ " ~"
posfixa (Mais x y) = (posfixa x) ++ " " ++ (posfixa y) ++ " +"
posfixa (Menos x y) = (posfixa x) ++ " " ++ (posfixa y) ++ " -"
posfixa (Mult x y) = (posfixa x) ++ " " ++ (posfixa y) ++ " *" 

--2----------

data RTree a = R a [RTree a]
    deriving (Show) 

--A
soma :: Num a => RTree a -> a
soma (R n l) = n + sum (map soma l)  

--C
prune :: Int -> RTree a -> RTree a 
prune n (R a l) | n == 1 = R a []
                | otherwise = R a (map (prune (n-1)) l) 

--D
mirror :: RTree a -> RTree a
mirror (R a l) = R a (reverse (map mirror l))

--E
postorder :: RTree a -> [a]
postorder (R a l) = concat (map postorder l) ++ [a]  

--3----------
data LTree a = Tip a | Fork (LTree a) (LTree a)

--A
ltsum :: Num a => LTree a -> a
ltsum (Tip x) = x
ltsum (Fork a b) = ltsum a + ltsum b   