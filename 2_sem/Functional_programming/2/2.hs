--1
evenNaturals :: Int -> [Int]
evenNaturals n = take n [2, 4 ..]

--2
pyramidalNumbers :: Int -> [Int]
pyramidalNumbers n = map (\k -> k * (k + 1) * (k + 2) `div` 6) [1..n]

--3
addLists :: [Int] -> [Int] -> [Int]
addLists [] ys = ys  
addLists xs [] = xs 
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys  


--4
substitute :: Char -> Char -> String -> String
substitute _ _ [] = []  
substitute old new (x:xs)
    | x == old  = new : substitute old new xs  
    | otherwise = x : substitute old new xs    