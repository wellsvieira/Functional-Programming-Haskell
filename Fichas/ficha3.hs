data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n s [] = [(n,[Email s])]
acrescEmail n s ((x,l):xs) | n==x = (x,(Email s):l) : xs
                           | otherwise = (x,l) : acrescEmail n s xs

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,l):xs) |n==x = Just (daListStr l)
                       |otherwise = verEmails n xs

daListStr :: [Contacto] -> [String]
daListStr [] = []
daListStr ((Email s):t) = s: daListStr t
daListStr (h:t) = daListStr t

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Email _):t) = consTelefs t
consTelefs ((Tlm x):t) = x: consTelefs t
consTelefs ((Trab x):t) = x: consTelefs t
consTelefs ((Casa x):t) = x: consTelefs t

casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((x,l):t) | n==x = numCasa l
                 | otherwise = casa n t

numCasa :: [Contacto] -> Maybe Integer
numCasa [] = Nothing
numCasa (h:t) = case h of
                  Casa x -> Just x
                  _      -> numCasa t