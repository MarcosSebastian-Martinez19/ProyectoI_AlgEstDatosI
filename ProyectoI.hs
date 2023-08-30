-- Ejercicio 1

-- A
esCero :: Int -> Bool
esCero x = x == 0

-- Prueba
--ghci> esCero 1
--False
--ghci> esCero 0
--True

-- B
esPositivo :: Int -> Bool
esPositivo x = x > 0

-- Prueba
--ghci> esPositivo (-1)
--False
--ghci> esPositivo 7
--True

-- C
esVocal :: Char -> Bool
esVocal x = x `elem` ['a', 'e', 'i', 'o', 'u']

-- Prueba
--ghci> esVocal 'a'
--True
--ghci> esVocal 'b'
--False

-- Ejercicio 2

-- A
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

-- Prueba
--ghci> paratodo [True, False, True]
--False
--ghci> paratodo [True, True]
--True

-- B
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- Prueba
--ghci> sumatoria [1, 5 ,(-4)]
--2
--ghci> sumatoria [10,20,34,2]
--66

-- C
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)

-- Prueba
--ghci> productoria [2, 4, 1]
--8
--ghci> productoria [2,5,6]
--60

-- D
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

-- Prueba
--ghci> factorial 5
--120
--ghci> factorial 2
--2

-- E
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs)

-- Prueba
--ghci> promedio [6,5,5,8]
--6
--ghci> promedio [6,5,6,9,9]
--7

-- Ejercicio 3

pertenece :: Int -> [Int] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x == y = True
                    | otherwise = pertenece x ys

-- Prueba
--ghci> pertenece 4 [2,4,6]
--True
--ghci> pertenece 6 [2,4,5]
--False

-- Ejercicio 4

-- A
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] f = True
paratodo' (x:xs) f = f x && paratodo' xs f

-- Prueba
--ghci> paratodo' [0,0,0,0] esCero
--True
--ghci> paratodo' "hola" esVocal
--False

-- B
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = f x || existe' xs f

-- Prueba
--ghci> existe' [0,0,1,0] esCero
--True
--ghci> existe' "tnt" esVocal
--False

-- C
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

-- Auxi1
por2 :: Int -> Int
por2 x = x*2

-- Prueba
--ghci> sumatoria' [1,2,3] por2
--12
--ghci> sumatoria' [15,2,5] por2
--44

-- D
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = f x * (productoria' xs f)

-- Prueba
--ghci> productoria' [1,2,3] por2
--48
--ghci> productoria' [3,4,10] por2
--960

-- Ejercicio 5

paratodo'' :: [Bool] -> Bool
paratodo'' xs = (paratodo' xs id)

-- Prueba
--ghci> paratodo'' [True, False, True]
--False
--ghci> paratodo'' [True, True]
--True

-- Ejercicio 6

-- A
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs esPar

-- Auxi2
esPar :: Int -> Bool
esPar x = mod x 2 == 0

-- Prueba
--ghci> todosPares [2,4,6]
--True
--ghci> todosPares [2,4,5]
--False

-- B
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo y xs = existe' xs (esMultiplo y)

-- Auxi3
esMultiplo :: Int -> Int -> Bool
esMultiplo n x = (mod x n == 0)

-- Prueba
--ghci> hayMultiplo 5 [15,25,31,48]
--True
--ghci> hayMultiplo 2 [9,145,11,23]
--False

-- C
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..(n-1)] (numeroCuadrado)

-- Auxi4
numeroCuadrado :: Int -> Int
numeroCuadrado n = n * n

-- Prueba
--ghci> sumaCuadrados 2
--1
--ghci> sumaCuadrados 5
--30

-- D
existeDivisor :: Int -> [Int] -> Bool
existeDivisor n (xs) = existe' xs (esDivisor n)

-- Auxi5
esDivisor :: Int -> Int -> Bool
esDivisor n m = (mod n m == 0)

-- Prueba
--ghci> existeDivisor 5 [10,49,35,80]
--False
--ghci> existeDivisor 8 [2,3,5,7,4]
--True

-- E
esPrimo:: Int -> Bool
esPrimo n = n > 1 && not (existeDivisor n [2..(n-1)])

-- Prueba
--ghci> esPrimo 5
--True
--ghci> esPrimo 10
--False

-- F
factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = productoria' [1..n] id
-- Prueba
--ghci> factorial' 5
--120
--ghci> factorial' 3
--6

-- G
multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = productoria' (listaPrimos xs) id

listaPrimos :: [Int] -> [Int]
listaPrimos [] = []
listaPrimos (x:xs) | (esPrimo x == True) = (x : listaPrimos xs)
                 | (esPrimo x == False) = listaPrimos xs 

-- Prueba
--ghci> multiplicaPrimos [1,2,6,5,7]
--70
--ghci> multiplicaPrimos [3,5,11]
--165

-- H
esFib :: Int -> Bool
esFib n = existe' (succFibonacci (n+1)) (==n)

succFibonacci :: Int -> [Int]
succFibonacci n = [fib n | n <- [0..n]]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1 
fib n = (fib (n-1) + fib (n-2))

-- Prueba
--ghci> esFib 2
--True
--ghci> esFib 5
--True
--ghci> esFib 9
--False
--ghci> esFib 11
--False

--I
todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib

-- Prueba
--ghci> todosFib [0,1,2,5,8,13,21,55,34]
--True
--ghci> todosFib [0,1,2,5,11]
--False

-- Ejercicio 7

-- Función map
-- Lo que hace esta función es dada una lista y una función te devuelve una lista con la función aplicada a cada uno de los elementos de la lista.
-- La expresión map succ [1, -4, 6, 2, -8], donde succ n = n + 1. Lo que pasa es que a cada elemento de la lista le aplica la función del sucesor y la lista resultante sería [2, -3, 7, 3, -7]

-- Función filter
-- Lo que hace esta función es dada una lista y una condición te devuelve una lista con los elementos que cumplen con la condición.
-- La expresión filter esPositivo [1, -4, 6. 2. -8]. Lo que pasa es que verifica que cada elemento de la lista sea positivo y los mantiene en la lista. Resultando la siguiente lista [1, 6, 2]

-- Ejercicio 8
duplicado :: [Int] -> [Int]
duplicado [] = []
duplicado (x:xs) = x * 2 : duplicado xs

-- Prueba
--ghci> duplicado [2,3,15,6]
--[4,6,30,12]
--ghci> duplicado [5,20,100]
--[10,40,200]

mapx2 :: [Int] -> [Int]
mapx2 xs = map (*2) xs

--Prueba
--ghci> mapx2 [2,3,15,6]
--[4,6,30,12]
--ghci> mapx2 [5,20,100]
--[10,40,200]


-- Ejercicio 9
filterPrimos :: [Int] -> [Int]
filterPrimos [] = []
filterPrimos (x:xs) | (esPrimo x == True) = (x : filterPrimos xs)
                 | (esPrimo x == False) = filterPrimos xs 

-- Prueba
--ghci> filterPrimos [1,5,2,3,17,11,20]
--[5,2,3,17,11]
--ghci> filterPrimos [8,9,3,7,15,13,19]
--[3,7,13,19]

filterPrimos' :: [Int] -> [Int]
filterPrimos' xs = filter (esPrimo) xs

-- Prueba
--ghci> filterPrimos' [1,5,2,3,17,11,20]
--[5,2,3,17,11]
--ghci> filterPrimos' [8,9,3,7,15,13,19]
--[3,7,13,19]

-- Ejercicio 9c
multiplicaPrimos' :: [Int] -> Int
multiplicaPrimos' xs = productoria (filter esPrimo (xs))

-- Prueba
--ghci> multiplicaPrimos' [1,2,6,5,7]
--70
--ghci> multiplicaPrimos' [3,5,11]
--165

-- Ejercicio 10

-- A
primIgualesA :: (Eq a) => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | (x == n) = x : (primIgualesA n xs)
                    | (x /= n) = []

-- Prueba
--ghci> primIgualesA 3 [3,3,4,1]
--[3,3]
--ghci> primIgualesA 3 [4,3,3,4,1]
--[]
--ghci> primIgualesA 3 []
--[]
--ghci> primIgualesA 'a' "aaadaa"
--"aaa"

-- B
primIgualesA' :: (Eq a) => a -> [a] -> [a]
primIgualesA' n = takeWhile (==n)

-- Prueba
--ghci> primIgualesA' 3 [3,3,4,1]
--[3,3]
--ghci> primIgualesA' 3 [4,3,3,4,1]
--[]
--ghci> primIgualesA' 3 []
--[]
--ghci> primIgualesA' 'a' "aaadaa"
--"aaa"

-- Ejercicio 11
-- A

primIguales :: (Eq a) => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:xs) | (x == (head xs)) = x : (primIguales xs)
                    | otherwise = [x]

-- Prueba
--ghci> primIguales [3,3,4,1]
--[3,3]
--ghci> primIguales [4,3,3,4,1]
--[4]
--ghci> primIguales []
--[]
--ghci> primIguales "aaadaa"
--"aaa"
--ghci> primIguales "a"
--"a"
--ghci> primIguales [4]
--[4]
--ghci> primIguales ""
--""

-- B

primIguales' :: (Eq a) => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA (x) (x:xs)

-- Prueba
--ghci> primIguales' [3,3,4,1]
--[3,3]
--ghci> primIguales' [4,3,3,4,1]
--[4]
--ghci> primIguales' []
--[]
--ghci> primIguales' "aaadaa"
--"aaa"
--ghci> primIguales' "a"
--"a"
--ghci> primIguales' [4]
--[4]
--ghci> primIguales' ""
--""

-- Ejercicio 12
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t = (t x) `op` (cuantGen op z xs t)

-- Prueba
--ghci> cuantGen (+) 0 [] (+0)
--0
--ghci> cuantGen (*) 1 [] (+0)
--1
--ghci> cuantGen (&&) True [] (==True)
--True
--ghci> cuantGen (||) False [] (==False)
--False

--ghci> cuantGen (+) 0 [1,3,5] (*2)
--18
--ghci> cuantGen (*) 1 [1,3,5] (*2)
--120
--ghci> cuantGen (&&) True [True, False, True] (==True)
--False
--ghci> cuantGen (&&) True [True, True, True] (==True)
--True
--ghci> cuantGen (||) False [False, True, False] (==False)
--True
--ghci> cuantGen (||) False [True, True, True] (==False)
--False


paratodo''' :: [a] -> (a -> Bool) -> Bool
paratodo''' xs f = cuantGen (&&) True xs f 

-- Prueba
--ghci> paratodo''' [0,0,0,0] esCero
--True
--ghci> paratodo''' "hola" esVocal
--False

existe''' :: [a] -> (a -> Bool) -> Bool
existe''' xs f = cuantGen (||) False xs f

-- Prueba
--ghci> existe''' [0,0,1,0] esCero
--True
--ghci> existe''' "tnt" esVocal
--False

sumatoria''' :: [a] -> (a -> Int) -> Int
sumatoria''' xs f = cuantGen (+) 0 xs f

-- Prueba
--ghci> sumatoria''' [1,2,3] (*2)
--12
--ghci> sumatoria''' [15,2,5] (*2)
--44

productoria''' :: [a] -> (a -> Int) -> Int
productoria''' xs f = cuantGen (*) 1 xs f

-- Prueba
--ghci> productoria''' [1,2,3] (*2)
--48
--ghci> productoria''' [3,4,10] (*2)
--960


-- Ejercicio 13
distanciaEdicion :: [Char] -> [Char] -> Int
distanciaEdicion [] ys = length ys
distanciaEdicion xs [] = length xs
distanciaEdicion (x:xs) (y:ys)  | x == y = distanciaEdicion xs ys
                                | x /= y = 1 + distanciaEdicion xs ys

-- Prueba
--ghci> distanciaEdicion "agua" "tierra"
--6
--ghci> distanciaEdicion "agua" "agua"
--0
--ghci> distanciaEdicion "agua" ""
--4
--ghci> distanciaEdicion "" "tierra"
--6

-- Ejercicio 14
primQueCumplen :: [a] -> ( a -> Bool ) -> [a]
primQueCumplen [] f = []
primQueCumplen (x:xs) f | (f x == True) = x : (primQueCumplen xs f)
                        | (f x == False) = [] 

-- Prueba
--ghci> primQueCumplen [0,1,2,3,4] (==0)
--[0]
--ghci> primQueCumplen [0,0,2,0,4] (==0)
--[0,0]
--ghci> primQueCumplen ['a','b','t','u'] esVocal
--"a"
--ghci> primQueCumplen ['c','b','t','u'] esVocal
--""
--ghci> primQueCumplen [] esVocal
--""
--ghci> primQueCumplen [] (==0)
--[]
