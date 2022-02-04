sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
  where
     go acc g []     = acc
     go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
  where
    go acc g []     = acc
    go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z []     = []
foldl' f z (x:xs) = foldl' f (f z x) xs

sumWith''' g = foldl' (\acc x -> g x + acc) 0
prodWIth''' g = foldl' (\acc x -> g x * acc) 1

map' g = foldr (\x xs -> g x : xs) []
map'' g = foldl (\acc x -> acc ++ [g x]) []

filter' g = foldr (\x xs -> if g x then x : xs else xs) []
filter'' g = foldl (\acc x -> if g x then acc ++ [x] else acc)

foldl'' g z = foldr (\x xs -> g xs x) z
foldr'' g z = foldl (\xs x -> g xs x) z