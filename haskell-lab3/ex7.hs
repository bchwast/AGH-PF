import Data.Char

onlyEven [] = []
onlyEven (x:xs)
  | x `mod` 2 == 0 = x : onlyEven xs
  | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
  | x `mod` 2 == 1 = x : onlyOdd xs
  | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
  | isUpper x = x : onlyUpper xs
  | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x == True = x : filter' p xs
  | otherwise   = filter' p xs

onlyEven' = filter' even
onlyOdd' = filter' odd
onlyUpper' = filter isUpper

exp1 = length $ onlyEven [1..10^6]

exp2 = length $ filter even [1..10^6]

exp3 = length [x | x <- [1..10^6], even x]