-------------2------------------------------------------
import Data.Char

dobros :: [Float] -> [Float] --------------a)
dobros [] = []
dobros (x:xs) = x**2 : dobros (xs)

numOcorre :: Char -> [Char] -> Int ----------------b)
numOcorre c [] = 0
numOcorre c (x:xs)   
         |c /= x = numOcorre c xs
         |toLower c == toLower x = 1 + numOcorre c xs

positivos :: [Int] -> Bool ----------------c)
positivos [] = True
positivos (x:xs) |x<0 = False 
                 |otherwise = positivos xs

soPos :: [Int] -> [Int] ----------------d)
soPos [] = []
soPos (x:xs) |x>0 = x : soPos xs
             |otherwise = soPos xs

somaNeg :: [Int] -> Int ----------------e)
somaNeg [] = 0
somaNeg (x:xs) |x<0 = x + somaNeg xs
               |otherwise = 0 + somaNeg xs

tresUlt :: [a] -> [a] ----------------f)
tresUlt l |length l <= 3 = l 
          |otherwise = tresUlt (tail l)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):xs) = y : segundos xs

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((x,y):xs)|a==x = True
                         |otherwise = nosPrimeiros a xs

sumTriplosauxiliar :: (Num a, Num b, Num c) => (a,b,c) -> (a,b,c) -> (a,b,c)
sumTriplosauxiliar (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos (z:zs) = sumTriplosauxiliar z (sumTriplos zs)

---------3--------------------------------------------------------

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) |isAlpha x == True = soDigitos xs
                 |otherwise = (x : soDigitos xs)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) |isLower x == True = 1 + minusculas xs
                  |otherwise = minusculas xs

nums :: String -> [Int]
nums [] = []
nums (x:xs) | isAlpha x == False = (digitToInt x : nums xs)
            | otherwise = nums xs

