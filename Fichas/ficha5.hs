--------------1-------------------
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs 

--b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

--c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) |f x = x: takeWhile' f xs
                    |otherwise = []

--d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) |f x = dropWhile' f xs
                   |otherwise = (x:xs)

--e)
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) |f x = (x:s1,s2)
               |otherwise = ([],x:xs)
    where (s1,s2) = span' f xs

--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f l (x:xs) |f l x = xs
                     |otherwise = x:deleteBy' f l xs
deleteBy' _ _ _ = []

--g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insere x (sortOn' f xs)
    where insere x [] = [x]
          insere x (h:t) |f x > f h = h:insere x t
                         |otherwise = x:h:t 

--------------2-------------------------------
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n l = filter (\x -> snd x == n) l

--b)
conta :: Int -> Polinomio -> Int
conta n l = length (filter (\x -> snd x == n) l)

--c)
grau :: Polinomio -> Int
grau (x:xs) = maximum(lnsd (x:xs))
    where lnsd [] = []  
          lnsd (x:xs) = snd x : lnsd xs 

------------3--------------------------------
--a)
type Mat a = [[a]]

dimOk :: Mat a -> Bool
dimOk l |mod (length(concat l)) (length l) == 0  = True
        |otherwise = False

--b)
dimMat :: Mat a -> (Int,Int)
dimMat l = (length l,)