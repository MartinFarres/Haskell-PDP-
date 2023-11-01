-- Funciones Recursivas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Char
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

main :: IO()
main = do
    print resultadoEjemplo

-- suma, que suma todos los elementos de una lista de números
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

-- alguno, que devuelve True si algún elemento de una lista de valores booleanos es True, y False en
-- caso contrario
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

-- todos, que devuelve True si todos los elementos de una lista de valores booleanos son True, y False
-- en caso contrario
todos :: [Bool] -> Bool
todos [] = False
todos [x] = x
todos (x:xs) = x && todos xs

-- codigos, que dada una lista de caracteres, devuelve la lista de sus ordinales
codigos :: [Char] -> [Int]
codigos [] = []
codigos (x:xs) = ord x : codigos xs

-- orden, que dada una lista de pares de números, devuelve la lista de aquellos pares en los que la
-- primera componente es menor que el triple de la segunda
orden :: [(Int, Int)] -> [(Int, Int)]
orden [] = []
orden ((x,y):xs)
    | x <= 3*y = (x,y) : orden xs
    | otherwise = orden xs

-- Definición de un operador que aplica una lista de funciones a un entero y devuelve la lista de enteros
-- de los resultados.
(°) :: Int -> [Int -> Int] -> [Int]
(°) _ [] = []
(°) n (f:fs) = f n : (°) n fs

funciones :: [Int -> Int]
funciones = [(+ 10), (* 2), subtract 5]
resultadoEjemplo :: [Int]
resultadoEjemplo = 5 ° funciones -- Aplicar las funciones a 5

-- Definir una función que devuelva la posición inicial de una sublista en una lista dada
posicionSublista :: Eq a => [a] -> [a] -> Maybe Int
posicionSublista _ [] = Just 0  -- Si la sublista es vacía, la posición es 0 (posición inicial)
posicionSublista [] _ = Nothing  -- Si la lista es vacía, la sublista no puede estar presente
posicionSublista lista@(x:xs) sublista@(y:ys) 
    | take (length sublista) lista == sublista = Just 0
    | otherwise = posicionSublista xs sublista

