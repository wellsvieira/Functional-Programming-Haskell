import System.Random
import Data.List
---------------------------------------1
bingo :: IO()
bingo = do nl <- acumularNumeros []
           print nl

acumularNumeros :: [Int] -> IO [Int]
acumularNumeros l | length l == 90 = do return l
                  | otherwise = do v <- randomRIO (1,90)
                                   print v
                                   getChar
                                   let nl = if v `elem` l then l else v:l in acumularNumeros nl


--mastermind :: IO ()
--mastermind = do n <- aux l
                --return n

aux :: [Int] -> IO [Int]
aux l = do n <- digits
           if l == n then return [] else



{-digits :: IO [Int]
digits = do x <- randomRIO(0,9)
            y <- randomRIO(0,9)
            z <- randomRIO(0,9)
            w <- randomRIO(0,9)
            return [x,y,z,w]
-}

-------------------------------------2
data Aposta = Ap [Int] (Int,Int) deriving Show

valida :: Aposta -> Bool
valida (Ap l (a,b)) = if length l == 5 && all (\y -> elem y [1..50]) l && a /= b && elem a l == False && elem b l==False && elem a [1..9] == True && elem b [1..9] ==True
    then True else False
valida _ = False

-----b)

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l1 (a,b)) (Ap l2 (c,d)) = (num,est) 
    where num = length(intersect l1 l2)
          est = length(intersect [a,b] [c,d])

-------c)
--i
instance Eq Aposta where
    Ap l1 e1 == Ap l2 e2 = comuns (a) (b) == (5,2)
        where a = Ap l1 e1
              b = Ap l2 e2
--ii
premio :: Aposta -> Aposta -> Maybe Int
premio a b |comuns a b == (5,2) = Just 1
           |comuns a b == (5,1) = Just 2
           |comuns a b == (5,0) = Just 3
           |comuns a b == (4,2) = Just 4
           |comuns a b == (4,1) = Just 5
           |comuns a b == (4,0) = Just 6
           |comuns a b == (3,2) = Just 7
           |comuns a b == (2,2) = Just 8
           |comuns a b == (3,1) = Just 9
           |comuns a b == (3,0) = Just 10
           |comuns a b == (1,2) = Just 11
           |comuns a b == (2,1) = Just 12
           |comuns a b == (2,0) = Just 13
           |otherwise = Nothing

-------d)
--i
leAposta :: IO Aposta
leAposta = do print "Introduza os numeros (na forma de uma lista)"
              num <- getLine
              print "Introduza as estrelas (na forma de produto cartesiano)"
              est <- getLine
              let bet = Ap (read num) (read est)
              if valida bet then return bet 
                else do print "A aposta produzida nao e valida,tente novamente!"; leAposta

--ii
joga :: Aposta -> IO ()
joga (Ap num est) |valida (Ap num est) == False = do return ()
                  |otherwise =
                      do c <-leAposta
                         let prize = (premio (Ap num est) c)
                         print "Parabens, ganhaste o premio:"
                         print prize
                         return ()

-------e)
geraChave :: IO Aposta
geraChave = do a <- randomRIO(1,9)
               b <- randomRIO(1,9)
               n1 <- randomRIO(1,50)
               n2 <- randomRIO(1,50)
               n3 <- randomRIO(1,50)
               n4 <- randomRIO(1,50)
               n5 <- randomRIO(1,50)
               let num = [n1,n2,n3,n4,n5]
               let est = (a,b)
               let ans = valida (Ap num est)
               if ans == True then return (Ap num est) else geraChave

-------f)
main :: IO ()
main = do ch <- geraChave
          ciclo ch

ciclo :: Aposta -> IO ()
ciclo ch = do menunum <- menu
              if menunum == "1" then do joga ch ; ciclo ch
              else if menunum == "2" then do print "Nova chave gerada"; main
              else return ()



menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
    where menutxt = unlines ["",
                             "Apostar ........... 1",
                             "Gerar nova chave .. 2",
                             "",
                             "Sair .............. 0"]