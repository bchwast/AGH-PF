import Data.Char

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x == True = x : filter' p xs
  | otherwise   = filter' p xs

onlyEven = filter' (\x -> x `mod` 2 == 0)
onlyOdd = filter' (\x -> x `mod` 2 == 1)
onlyUpper = filter isUpper

