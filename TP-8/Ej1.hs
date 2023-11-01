import Distribution.Simple.Utils (xargs)
import Text.XHtml (base)
main =
    -- print(volEsfera 4)  16.755161
    --print(sumaCoins 13 36 21 10 6)  15.030001
    print (myXOR False True)


-- Volumen de una esfera
volEsfera :: Float -> Float
volEsfera r = r * pi * 4/3

-- La suma de monedas tal que (sumaCoins a b c d e) es la suma de los pesos correspondientes a: a
-- monedas de 1 centavo, b de 5 centavos, c de 10 centavos, d de 50 centavos, e de 1 peso.
sumaCoins :: Float -> Float -> Float -> Float -> Float -> Float
sumaCoins a b c d e = e + d/2 + c/10 + b/20 + a/100

-- Incrementar todos los elementos de una tupla de tres enteros.
incrTuple :: (Int,Int,Int) -> (Int,Int,Int)
incrTuple (a,b,c) = (a+1, b+1, c+1)

-- Calcular el cuadrado de un número
pwr2 :: Int -> Int
pwr2 x = x * x

-- calcular el valor de un número elevado a la 4 utilizando la función del inciso anterior
pwr4 :: Int -> Int
pwr4 x = pwr2 x * pwr2 x

-- Calcular el máximo entre 3 números, puede utilizar la función predefinida max
myMax3 :: Int -> Int -> Int -> Int
myMax3 a b c
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

-- Calcular el máximo entre 6 números, puede utilizar la función predefinida max
myMax6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
myMax6 a b c d e f = max (myMax3 a b c) (myMax3 d e f)

-- Definir las raíces de una ecuación de segundo grado
roots :: Double -> Double -> Double -> Maybe(Int, Int)
roots a b c
    | discriminant < 0 = Nothing
    | otherwise = Just (root1, root2)
    where
        discriminant = b * b - 4 * a * c 
        root1 = round $ (b + sqrt discriminant) / (2 * a)
        root2 = round $ (b - sqrt discriminant) / (2 * a)


-- Definir el operador XOR
myXOR :: Bool -> Bool -> Bool
myXOR p q
    | (p && not q) || (not p && q) = True
    | otherwise = False

