average :: [Double] -> Double
average xs = 
    let (sum, count) = foldl (\(s, c) x -> (s + x, c + 1)) (0, 0) xs
    in sum / count
--* и +
dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = foldr (+) 0 (zipWith (*) xs ys)
--справ на лев
countEven :: [Int] -> Int
countEven = foldr (\x cnt -> if even x then cnt + 1 else cnt) 0
--сорт
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

quicksortBy :: (a -> a -> Bool) -> [a] -> [a]
quicksortBy _ [] = []
quicksortBy cmp (x:xs) = 
    let smaller = quicksortBy cmp [a | a <- xs, cmp a x]
        bigger  = quicksortBy cmp [a | a <- xs, not (cmp a x)]
    in smaller ++ [x] ++ bigger