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
sumSqr = sumWith (^2)
sumCube = sumWith (^3)
sumAbs = sumWith abs

listLength = sumWith (const 1)

prod' :: Num a => [a] -> a
prod' []     = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f []     = 1
prodWith f (x:xs) = f x * prodWith f xs

prod = prodWith id
prodSqr = prodWith (^2)
prodCube = prodWith (^3)
prodAbs = prodWith abs

genWith :: Num a => (a -> a) -> (a -> a -> a) -> [a] -> a
genWith f g (x:xs) = loop f g (f x) xs
  where loop f g acc []     = acc
        loop f g acc (x:xs) = loop f g (g (f x) acc) xs

