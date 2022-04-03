------1-------------
----a)
data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | x < y = mdc x (y-x)

normaliza :: Frac -> Frac
normaliza (F x y) | x>=0 && y>0 = F (div x d) (div y d)
                  | x<0 && y>0 = F (div (abs x) d) (div (abs y) d)
                  | otherwise = F (div (-(abs x)) d) (div (abs y) d)
    where d = mdc (abs x) (abs y)

----b)
instance Eq Frac where
    (F a b) == (F c d) = a*d == c*b

--or-----
    --f1==f2 = let (F a b) = normaliza f1
    ---------------(F c d) = normaliza f2
    ---------------in a==c && b==d

----c)
instance Ord Frac where
    (F a b) <= (F c d) = a*d <= c*b

----d)
instance Show Frac where
    show (F x y) = "(" ++ show x ++ "/" ++ show y ++ ")"
----e)
instance Num Frac where
    (F a b) + (F c d) = F (a*d + c*b) (b*d)
    (F a b) - (F c d) = F (a*d - c*b) (b*d) 
    (F a b) * (F c d) = F (a*c) (b*d)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a == 0 = F 0 1
                   | a * b > 0 = 1
                   | otherwise = -1
    fromInteger x = F x 1

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
