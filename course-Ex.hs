{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
main =
    print(hasPath [(1,2), (2,3), (2, 1), (4, 5), (5, 2)] 5 1)

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = (e == x) || myElem e xs

myNub :: (Eq a) => [a] -> [a]
myNub [] = []
myNub (x:xs)
    | myElem x xs = myNub xs
    | otherwise = x : myNub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:y:xs)
    | x <= y = isAsc(y:xs)
    | otherwise = False


-- Create a function hasPath that determines if a path
-- from one node to another exists within a directed graph
-- Inp: [(1,3), (2,4), ...] 2 3
hasPath :: [(Int, Int)] -> Int -> Int -> Bool  
hasPath [] x y = x == y
hasPath xs x y
    | x == y = True
    | otherwise = 
        let xs' = [(n,m)|(n,m) <- xs, n /= x] in --Deletes all visited nodes
            or [hasPath xs' m y | (n,m) <- xs, x == n] --Start to check from adjacent nodes
