import Data.List (sortBy)
import Distribution.Simple.Utils (xargs)
-- Combinacion de tipos

main :: IO ()
main = do
    let fecha = Date 20 3 1990
    let personasOrd = takeNombresPersonas personas fecha
    print personasOrd

personas :: [Persona]
personas =
    [ Persona "Juan" "Pérez" (Date 10 5 1970)
    , Persona "María" "González" (Date 15 8 1985)
    , Persona "Carlos" "López" (Date 3 1 1995)
    , Persona "Laura" "Martínez" (Date 20 2 1995)
    ]



-- SumaPar: dada una lista de pares, devuelve una nueva lista en la que cada elemento es la suma de
-- los elementos de cada par.
sumaPar :: [(Int, Int)] -> [Int]
sumaPar xs = [x+y| (x,y) <- xs]

sumaPar2 :: [(Int, Int)] -> [Int]
sumaPar2 [] = []
sumaPar2 ((a, b):xs) = (a+b) : sumaPar2 xs

-- zipMaximos: dadas dos listas de enteros, devuelve una lista donde el elemento n es el máximo entre
-- el elemento n de la lista 1 y de la lista 2.
zipMaximos :: [Int]->[Int]->[Int]
zipMaximos x [] = x
zipMaximos [] x = x
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys

-- ZipSort: dadas dos listas de enteros de igual longitud, devuelve una lista de pares (min,max), donde
-- min y max son el mínimo y el máximo entre los elementos de ambas listas en la misma posición.
zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] [] = []
zipSort (x:xs) (y:ys)
    | x <= y = (x, y) : zipSort xs ys
    | otherwise = (y, x) : zipSort xs ys


-- takePersonas: dada una lista de Personas [nombre, apellido y fecha de nacimiento] (también declare
-- un tipo de dato Date) ordenada ascende*+ntemente por fecha de nacimiento; y una fecha, devuelve
-- el segmento más largo de la lista con las personas que nacieron antes dicha fecha.1
data Date = Date {dia :: Int, mes :: Int, año :: Int} deriving Show
mayorDate :: Date -> Date -> Bool
mayorDate (Date d1 m1 a1) (Date d2 m2 a2)
    | a1 /= a2 = a1 > a2
    | m1 /= m2 = m1 > m2
    | otherwise = d1 > d2
data Persona = Persona { nombre :: String, apellido :: String, fechaNacimiento :: Date } deriving Show
takePersonas :: [Persona] -> Date -> [Persona]
takePersonas xs toComp = filter (\(Persona _ _ fechaNacimiento) -> mayorDate toComp fechaNacimiento ) xs

-- dropPrecio: dada una lista de Pizzas [lista de ingredientes y precio] en orden ascendente por precio,
-- dropPrecio devuelve el segmento más largo de la lista que comienza con la pizza que tiene el menor
-- precio superior a $200
data Pizza = Pizza {ingredientes :: [String], precio :: Int} deriving Show
dropPrecio :: [Pizza] -> [Pizza]
dropPrecio xs = sortBy (\p1 p2 -> compare (precio p1) (precio p2)) (filter (\(Pizza _ p) -> p > 200) xs)
pizzas :: [Pizza]
pizzas =
    [ Pizza ["Queso", "Jamón"] 180
    , Pizza ["Pepperoni", "Champiñones"] 250
    , Pizza ["Pimientos", "Cebolla", "Jalapeños"] 180
    , Pizza ["Pollo", "Aceitunas", "Tomate"] 300
    , Pizza ["Anchoas", "Cebolla"] 220
    ]

-- takeNombresPersonas: dada una lista de Personas y una fecha devuelve los nombres de las personas
-- incluidas en segmento más largo de la lista con las personas que nacieron antes dicha fecha
takeNombresPersonas :: [Persona] -> Date ->[String]
takeNombresPersonas xs fecha = [nombre| (Persona nombre _ _) <- takePersonas xs fecha ]



