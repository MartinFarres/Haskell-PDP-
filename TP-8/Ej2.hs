{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
main =
    print (reversa' (rango 1 9))




-- Listas

-- Definir la función divisores 
divisores :: Int -> [Int]
divisores n = [x | x <- [-abs n .. abs n], x /= 0 , mod n x == 0]

-- Dada una cantidad de segundos, devuelve la cantidad de horas, minutos y segundos equivalente.
segundosAHMS :: Int -> (Int, Int, Int)
segundosAHMS segundos = (horas, minutos, restanteSegundos)
  where
    horas = segundos `div` 3600
    segundosRestantes = segundos `mod` 3600
    minutos = segundosRestantes `div` 60
    restanteSegundos = segundosRestantes `mod` 60

-- Definir la función nIndex tal que nIndex l n es elemento enésimo de l, empezando a numerar con el 0.
nIndex :: [a] -> Int -> a
nIndex (x:_) 0 = x
nIndex (_:xs) n = nIndex xs (n-1)
nIndex [] _ = error "Index out of range"

-- Redefinir la función elem tal que elem e l se verifica si e es un elemento de 
myElem :: Eq e => e -> [e] -> Bool
myElem e [] = False
myElem e (x:xs) = x == e || myElem e xs

-- Definir la función que convierte el número decimal en su correspondiente número binario. Los nú-
-- meros binarios deben almacenarse como una lista
decimalABinario :: Int -> [Int]
decimalABinario 0 = [0]
decimalABinario 1 = [1]
decimalABinario n
    | n `mod` 2 == 0 = decimalABinario (n `div` 2) ++ [0]
    | otherwise = decimalABinario (n `div` 2) ++ [1]


-- Definir un nuevo tipo de datos para un número complejo y algunas operaciones básicas como la suma
-- y la multiplicación de números complejos
data ComplexNum = Imaginario Int Int  deriving (Show)
sumImaginarios :: ComplexNum -> ComplexNum -> ComplexNum
sumImaginarios (Imaginario a b) (Imaginario c d) = Imaginario (a+c) (b+d)
multImaginarios :: ComplexNum -> ComplexNum -> ComplexNum
multImaginarios (Imaginario a b) (Imaginario c d) = Imaginario ((a*c)-(b*d)) ((a*d)+(c*b))

-- Definir al menos 10 colores definiendo su propio tipo de datos. Ingresar la lista y evaluar si la lista
-- tiene los colores predreversa' (h:xs) = [x:h | x <- xs]efinidos por el usuario
data Color = Red | Blue | Orange | Black | White | Brown | Pink | Green | Yellow | Purple deriving (Eq, Show)
coloresDefinidos :: [Color] -> Bool
coloresDefinidos = all (\color -> color `elem` [Red, Blue, Orange, Black, White, Brown, Brown, Pink, Green, Yellow, Purple])

-- Crear una función ocurrencias, que toma un elemento y una lista y devuelve el número de ocurrencias
-- del elemento en la lista
ocurrencias :: Eq a => [a] -> a -> Int
ocurrencias [] _ = 0
ocurrencias (x:xs) a
    | x == a = 1 + ocurrencias xs a
    | otherwise = 0 + ocurrencias xs a

-- Escribir un programa que genere todas las permutaciones de n objetos diferentes (se ingresan los
-- datos como una lista).
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete elem (x:xs)
    | elem == x = xs
    | otherwise = x : delete elem xs
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x : ys | x <- xs, ys <- permutaciones (delete x xs)]

-- Crear una lista que contenga todos los enteros dentro de un rango dado
rango :: Int -> Int -> [Int]
rango a b
    | a == b = [a]
    | otherwise = a : rango (a+1) b

-- Generar las combinaciones de K objetos distintos elegidos de los N elementos de una lista
combinaciones :: Eq a => Int -> [a] -> [[a]]
combinaciones 0 _ = [[]]  -- Caso base: combinación de 0 elementos es una lista vacía
combinaciones _ [] = []  -- Caso base: lista vacía no tiene combinaciones
combinaciones k (x:xs) = combinaciones k xs ++ map (x:) (combinaciones (k - 1) xs)

-- mapToSucesor: dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
mapToSucesor :: [Int] -> [Int]
mapToSucesor xs = [x+1| x <- xs]

mapToSucesor2 :: [Int] -> [Int]
mapToSucesor2 [] = []
mapToSucesor2 x = map succ x

-- filtrarPositivos: dada una lista de enteros, devuelve una lista con los elementos que son positivos.
filtrarPositivos :: [Int] -> [Int]
filtrarPositivos xs = [x| x <- xs, x > 0]

filtrarPositivos2 :: [Int] -> [Int]
filtrarPositivos2 = filter (> 0)

-- Reversa: dada una lista de enteros, devuelve la lista con los mismos elementos de atrás para adelante.
reversa' :: [Int] -> [Int]
reversa' [] = []
reversa' (x:xs) = reversa' xs ++ [x]
