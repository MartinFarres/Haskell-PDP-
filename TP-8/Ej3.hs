-- Combinacion de tipos

main :: IO ()
main = do
    let fechaComparacion = Date 1 1 1990
    let personasFiltradas = takePersonas personas fechaComparacion
    print personasFiltradas

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
takePersonas xs toComp = filter(\(Persona _ _ fechaNacimiento) -> mayorDate toComp fechaNacimiento ) xs


