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
--existe' (x:xs) f = f x || existe' xs f
--esFib :: Int -> Bool
--esFib n = existe' succFibonacci 

--succFibonacci :: [Int]
--succFibonacci = [fib n | n <- [0..10]]

--fib :: Int -> Int
--fib 0 = 0
--fib 1 = 1 
--fib n = (fib (n-1) + fib (n-2))

-- 8
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


-- 9
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

--9c
multiplicaPrimos' :: [Int] -> Int
multiplicaPrimos' xs = productoria (filter esPrimo (xs))

-- Prueba
--ghci> multiplicaPrimos' [1,2,6,5,7]
--70
--ghci> multiplicaPrimos' [3,5,11]
--165

-- 10

primIgualesA :: (Eq a) => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | (x == n) = x : (primIgualesA n xs)
                    | (x /= n) = []

--ghci> primIgualesA 3 [3,3,4,1]
--[3,3]
--ghci> primIgualesA 3 [4,3,3,4,1]
--[]
--ghci> primIgualesA 3 []
--[]
--ghci> primIgualesA 'a' "aaadaa"
--"aaa"