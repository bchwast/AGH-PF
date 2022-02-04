fib :: (Num a, Eq a) => a -> a
fib n =
  if n == 0 || n == 1 then n
  else fib (n - 2) + fib (n - 1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

fib2 n = fibs !! n

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' []      = 1
prod' (x:xs)  = x * prod' xs

length' :: Num a => [a] -> a
length' [] = 0
length' s = 1 + length' (tail s)

or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

doubleAll :: Num t => [t] -> [t]
doubleAll []     = []
doubleAll (x:xs) = [2 * x] ++ doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll []     = []
squareAll (x:xs) = [x * x] ++ squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven []     = []
selectEven (x:xs) = if x `mod` 2 == 0 then [x] ++ selectEven xs
                    else selectEven xs

arth :: (Real a, Fractional b) => [a] -> b
arth [] = 0
arth s  = realToFrac (sum s) / fromIntegral (length s)

geo :: (Real a, Fractional b) => [a] -> b
geo [] = 0
geo s  = realToFrac (realToFrac (product s) ** (1 / fromIntegral (length s)))

getAvg:: (Real a, Fractional b) => [a] -> (b, b)
getAvg [] = (0, 0)
getAvg s  = (arth s, geo s)