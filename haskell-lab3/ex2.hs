sum' :: Num a => [a] -> a
sum' []      = 0
sum' (x:xs)  = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []      = 0
sumSqr' (x:xs)  = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f []     = 0
sumWith f (x:xs) = f x + sumWith f xs

sum'' = sumWith id
sumSqr = sumWith sqrt
sumCube = sumWith (^3)
sumAbs = sumWith abs