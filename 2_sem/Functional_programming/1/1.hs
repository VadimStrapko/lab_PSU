example :: ([Integer], [Double], [(Bool, Char)])
example = ([1, 2, 3], [3.14, 2.71, 1.618], [(True, 'a'), (False, 'b'), (True, 'c')])

sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 x y = if x < y then (x, y) else (y, x)