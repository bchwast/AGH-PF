a = length [(a, b, c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: Integral t => t -> Bool
isPrime n
    | n < 2                             = False
    | n == 2 || n == 3                  = True
    | n `mod` 2 == 0 || n `mod` 3 == 0  = False
    | otherwise                         = [i | i <- [2..n-1], n `mod` i == 0] == []

b = length [i | i <- [1..10000], isPrime i]

allEqual :: Eq a => [a] -> Bool
allEqual xs
    | xs == []                                          = True
    | length xs == 1                                    = True
    | head xs == head (tail xs) && allEqual (tail xs)   = True
    | otherwise                                         = False