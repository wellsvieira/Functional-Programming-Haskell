module Testquestions where

enumfromto :: Int -> Int -> [Int] ------- Questão 1
enumfromto m n | m > n = []
               | m <= n = m : enumfromto (m+1) n
enumfromtoThen2 :: Int -> Int -> Int -> [Int] ------------Questão 2
enumfromtoThen2 p d u
           | d == 0 = []
           | u < d = [p]
           | otherwise = p : enumfromtoThen2 d (2 * d - p) u

merge :: [a] -> [a] -> [a] ---------Questão 3
merge [] l = l
merge (x:xs) l = x : (merge xs l)

position :: Int -> [a] -> a -----------Questão 4
position 0 (x:xs) = x
position i (x:xs) = position (i-1) xs

reverse0 :: [a] -> [a] ----------------Questão 5
reverse0 [] = []
reverse0 (x:xs) = merge (reverse0 xs)  [x]

take1 :: Int -> [a] -> [a] -----------------Questão 6
take1 0 (x:xs) = []
take1 a (x:xs) | length (x:xs)<a = (x:xs) 
               | otherwise = x : take1 (a-1) xs

drop1 :: Int -> [a] -> [a] -----------------Questão 7
drop1 0 (x:xs) = (x:xs)
drop1 a (x:xs) | length (x:xs)<=a = []
               | otherwise = drop1 (a-1) xs

zip1 :: [a] -> [b] -> [(a,b)] ---------------Questão 8
zip1 [] l1 = []
zip1 l2 [] = []
zip1 (x:xs) (y:ys) = ((x,y):zip1 xs ys)

replicate1 :: Int -> a -> [a] -----------------Questão 9
replicate1 0 x = []
replicate1 n x = x: replicate1 (n-1) x

intersperse1 :: a -> [a] -> [a] ----------------Questão 10
intersperse1 n [x] = [x]
intersperse1 n (x:xs) = x : n : intersperse1 n xs

group1 :: Eq a => [a] -> [[a]] -----------------Questão 11-----(IMP)
group1 [] = []
group1 (h:t) = group1aux h (group1 t)

group1aux :: Eq a => a -> [[a]] -> [[a]]
group1aux x [] = [[x]]
group1aux x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)

concat1 :: [[a]] -> [a] -----------------------Questão 12
concat1 [] = []
concat1 (h:t) = merge h (concat1 t)

inits1 :: [a] -> [[a]] -----------------Questão 13------(IMP)
inits1 [] = [[]]
inits1 l = merge (inits1 (init l)) [l]

tails1 :: [a] -> [[a]] -----------------Questão 14
tails1 [] = [[]]
tails1 (x:xs) = merge [(x:xs)] (tails1 xs)

heads1 :: [[a]] -> [a] -----------------Questão 15
heads1 [] = []
heads1 (xs:ys) |length xs == 0 = heads1 ys
               |otherwise = head xs : heads1 ys

total1 :: [[a]] -> Int --------------------Questão 16
total1 [] = 0
total1 (xs:ys) = length1 xs + total1 ys

length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

fun :: [(a,b,c)] -> [(a,c)] -----------------Questão 17
fun [] = []
fun ((x,y,z):xyzs) = ((x,z):fun xyzs)

cola :: [(String,b,c)] -> String --------------- Questão 18
cola [] = []
cola ((x,y,z):xyzs) = merge x (cola xyzs)

idade :: Int -> Int -> [(String,Int)] -> [String] -------- Questão 19
idade a i [] = []
idade a i ((x,y):xys) | y+i<=a = x: idade a i xys
                      | otherwise = idade a i xys 

powerEnumFrom :: Int -> Int -> [Int] --------------------Questão 20
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                  |otherwise = []

isPrime :: Int -> Bool ---------------------- Questão 21---(IMP)
isPrime 2 = True
isPrime n
    | n > 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True -- equivalente a: m > √n (assim trabalhamos apenas com valores inteiros)
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)
-- `primeCheck` percorre os números de 2 a √n e verifica se algum destes divide n com resto 0.
-- Caso tal não se verifique para nenhum destes valores, n é primo.

isPrefix :: Eq a => [a] -> [a] -> Bool ------------ Questão 22
isPrefix l1 [] = False
isPrefix [] l2 = True
isPrefix l1 l2 | l1 == l2 = True
               | otherwise = isPrefix l1 (init(l2))

isSuffix :: Eq a => [a] -> [a] -> Bool ----------------Questão 23
isSuffix l1 [] = False
isSuffix [] l2 = True
isSuffix l1 (x:xs) |l1 == (x:xs) = True
                   |otherwise = isSuffix l1 xs

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool -------Questão 24----(IMP) (Dúvida)
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (h':t') = h == h' && isSubsequenceOf t t' || isSubsequenceOf (h:t) t'

elemindices :: Eq a => a -> [a] -> [Int] ------------Questão 25-----(IMP)
elemindices n [] = []
elemindices n xs = elemaux n xs 0

elemaux :: Eq a => a -> [a] -> Int -> [Int]
elemaux m [] i = []
elemaux m (x:xs) i | m==x = i:elemaux m xs (i+1)
                   | otherwise = elemaux m xs (i+1)
-----Importante-------
init1 :: [a] -> [a] --------------Auxiliar para inits1
init1 [_] = []
init1 (x:xs) = x : init1 xs

nub :: Eq a => [a] -> [a] ---------------Questão 26---(IMP)
nub [] = []
nub (h:t) = h : filter (/= h) (nub t)

delete1 :: Eq a => a -> [a] -> [a] ---------------Questão 27
delete1 n [] = []
delete1 n (x:xs) |n==x = xs
                 |otherwise = x: delete1 n xs

takenum :: Eq a => [a] -> [a] -> [a] ----------------Questão 28
takenum l1 [] = l1
takenum (x:xs) (y:ys) | x == y = takenum xs ys
                      |otherwise = x: takenum xs (y:ys)


groupnecc :: Eq a => [a] -> [[a]] -----------NECC RESOLUTION
groupnecc [] = []
groupnecc [h] = [[h]]
groupnecc (h:t) = let ((x:xs):ys) = groupnecc t
                in if h==x then (h:x:xs):ys else [h]:(x:xs):ys

union1 :: Eq a => [a] -> [a] -> [a] --------------Questão 29
union1 l [] = l
union1 l (y:ys) |elem y l = union1 l ys
                | otherwise = union1 (l ++ [y]) ys

intersect1 :: Eq a => [a] -> [a] -> [a] ------------Questão 30
intersect1 [] l = []
intersect1 (x:xs) l | elem x l  = x : intersect1 xs l
                    | otherwise = intersect1 xs l

insert1 :: Ord a => a -> [a] -> [a] ----------------Questão 31
insert1 n [] = [n]
insert1 n (x:xs) | n>=x = x: insert1 n xs
                 |otherwise = n : (x:xs)

unwords1 :: [String] -> String -------------------Questão 32
unwords1 (x:xs) |null xs = x
                |otherwise = x ++ " " ++ unwords1 xs


unlines1 :: [String] -> String -------------Questão 33
unlines1 [] = []
unlines1 (x:xs) = x ++ "\n" ++ unlines1 xs

pMaior :: Ord a => [a] -> Int ----------------Questão 34------(IMP)
pMaior (y:ys) = pMaiorAux (y,0,1) ys

pMaiorAux :: Ord a => (a,Int,Int) -> [a] -> Int
pMaiorAux (m,pm,pa) [] = pm
pMaiorAux (m,pm,pa) (x:xs)|x>m = pMaiorAux (x,pa,(pa+1)) xs
                          |otherwise = pMaiorAux (m,pm,(pa+1)) xs 

lookup1 :: Eq a => a -> [(a,b)] -> Maybe b -------------Questão 35
lookup1 c [] = Nothing
lookup1 c ((x,y):xys) |c==x = Just y
                      |otherwise = lookup1 c xys 

precrescente :: Ord a => [a] -> [a] ---------------------Questão 36
precrescente [] = []
precrescente (x:y:xys) |x<=y = x:precrescente (y:xys)
                       |otherwise = [x]
                       

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert1 x (isort xs)
 
menor1 :: String -> String -> Bool ---------------Questão 38
menor1 l [] = False
menor1 [] m = True
menor1 (x:xs) (y:ys) = menor1 xs ys

elemSet :: Eq a => a -> [(a,Int)] -> Bool ------------Questão 39
elemSet c [] = False
elemSet c ((x,y):xys) |c==x = True
                      |otherwise = elemSet c xys

convertmset :: [(a,Int)] -> [a] --------------------Questão 40
convertmset [] = []
convertmset ((x,y):xys) |y==0 = convertmset xys
                        |otherwise = x : convertmset ((x,(y-1)):xys)

inseremset :: Eq a => a -> [(a,Int)] -> [(a,Int)] --------------Questão 41
inseremset n [] = [(n,1)]
inseremset n ((x,y):xys) |n==x = ((x,(y+1)):xys)
                         |otherwise = (x,y) : inseremset n xys

removemset :: Eq a => a -> [(a,Int)] -> [(a,Int)] ---------------Questão 42
removemset n [] = []
removemset n ((x,y):xys) |n==x && y==1 = removemset n xys
                         |n==x && y>1 = ((x,(y-1)):xys)
                         |otherwise = (x,y) : removemset n xys

constroimset :: Ord a => [a] -> [(a,Int)] --------------------Questão 43
constroimset [] = []
constroimset l = translate l 1

translate :: Ord a => [a] -> Int -> [(a,Int)]
translate [x] i = [(x,i)]
translate (x:y:xs) i |x<y = (x,i) : translate (y:xs) 1
                     |otherwise = translate (y:xs) (i+1)

partitiioneithers :: [Either a b] -> ([a],[b]) -----------------Questão 44--(IMP)
partitiioneithers (x:xs) = (left (x:xs),right (x:xs))
    where left [] = []
          left (Left x :xs) = x : left xs
          left (Right x :xs) = left xs
          right [] = []
          right (Right x :xs) = x : right xs
          right (Left x :xs) = right xs

mycatMaybes :: [Maybe a] -> [a] --------------------------Questão 45
mycatMaybes [] = []
mycatMaybes (Just x:xs) = x : mycatMaybes xs
mycatMaybes (Nothing : xs) = mycatMaybes xs

data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento] ---------------------Questão 46
caminho (x,y) (w,z) |x==w && y==z = []
                    |x==w && y<z = Norte : caminho (x,(y+1)) (w,z)
                    |x==w && y>z = Sul : caminho (x,(y-1)) (w,z)
                    |x<w = Este : caminho ((x+1),y) (w,z)
                    |x>w = Oeste : caminho ((x-1),y) (w,z)

hasLoops :: (Int,Int) -> [Movimento] -> Bool --------------------Questão 47---(IMP)
hasLoops _ [] = False
hasLoops n ms = n == posicao n ms || hasLoops n (init ms) 


posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao n [] = n
posicao (x,y) (Norte :ms) = posicao (x,(y+1)) ms
posicao (x,y) (Sul :ms) = posicao (x,(y-1)) ms
posicao (x,y) (Este :ms) = posicao ((x+1),y) ms
posicao (x,y) (Oeste :ms) = posicao ((x-1),y) ms

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int ---------------------Questão 48
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (w,z)):rects) | abs(w-x) == abs(z-y) = 1 + contaQuadrados rects
                                          | otherwise = contaQuadrados rects

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (w,z)):rects) = (abs (x-w) * abs(y-w)) + areaTotal rects



data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int ------------------------Questão 50
naoReparar [] = 0
naoReparar xs = naoRepararaux xs 0

naoRepararaux :: [Equipamento] -> Int -> Int
naoRepararaux [] n = n
naoRepararaux (Avariado: xs) n = naoRepararaux xs n
naoRepararaux (Bom: xs) n = naoRepararaux xs (n+1)
naoRepararaux (Razoavel: xs) n = naoRepararaux xs (n+1)