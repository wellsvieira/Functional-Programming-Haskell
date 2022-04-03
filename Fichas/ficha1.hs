max2 :: Int-> Int -> Int
max2 x y = if x >= y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z= if max2 x y >= z then max2 x y else z

perimetro :: Float -> Float
perimetro x = 2 * x * 3.14

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x,y) (z,w) = sqrt(((x-z)*(x-z))+((y-w)*(y-w)))

primUlt :: [Int] -> (Int,Int)
primUlt x = (head x,last x)

multiplo :: Int -> Int -> Bool
multiplo m n = 
    if mod m n == 0 then True
    else False

impar :: (Int) -> Bool ---------- Test if x number is odd 
impar n = 
    if mod n 2 == 0 then False
    else True

truncaImpar :: [Int] -> [Int]
truncaImpar x = 
    if impar (length x) == True then tail x
    else x

-------- 3--------------------------------
type Hora = (Int,Int) 
data HORA = H Int Int deriving (Show,Eq)

comphora :: Hora -> Hora -> Bool ----------- b)
comphora (h1,m1) (h2,m2) =
    if h1>h2 then True
    else False

convertermin :: Int -> Hora ----------------d)
convertermin min  = (div min 60,mod min 60)

difmin :: Hora -> Hora -> Int ------------------------------e)
difmin (h1,m1) (h2,m2) = if m1<m2 then (h1-h2)*60 -(m2-m1)
                        else (h1-h2)*60 + (m2-m1)

addmin:: Hora -> Int -> Hora -----------------------f)
addmin (h,m) x =
    if (h + div (m+a) 60) >= 24
    then ((h+(div (m+a) 60)-24),(mod (m+a) 60))
    else ((h+div (m+a) 60),(mod (m+a) 60))



-------5---------------------------------





