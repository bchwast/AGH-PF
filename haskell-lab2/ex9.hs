qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where
    leftPart xs  = [y | y <- xs, y <= x]
    rightPart xs = [y | y <- xs, y > x]

qSort' :: Ord a => [a] -> [a]
qSort' []     = []
qSort' (x:xs) = qSort' (leftPart xs) ++ [x] ++ qSort' (rightPart xs)
  where
    leftPart xs  = filter (<= x) xs
    rightPart xs = filter (> x) xs

mSort :: Ord a => [a] -> [a]
mSort []  = []
mSort [a] = [a]
mSort xs = merge (mSort leftPart) (mSort rightPart)
  where
    merge xs []         = xs
    merge [] xs         = xs
    merge (x:xs) (y:ys)
      | x < y     = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

iSort :: Ord a => [a] -> [a]
iSort []     = []
iSort (x:xs) = insert x (iSort xs)
  where
    insert i []     = [i]
    insert i (y:ys)
      | i > y     = y : insert i ys
      | otherwise = i : y : ys

concat' :: [[a]] -> [a]
concat xs = [y | x <- xs, y <- x]

concat'' :: [[a]] -> [a]
concat'' []     = []
concat'' (x:xs) = x ++ concat'' xs

isSorted :: [Int] -> [Bool]
isSorted []     = True
isSorted [a]    = True
isSorted (x:xs) = x <= head xs && isSorted xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = (map fst xs, map snd xs)

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _               = []
zip3' _ [] _               = []
zip3' _ _ []               = []
zip3' (x:xs) (y:ys) (z:zs) = zip3' xs ys zs

subList :: Eq a => [a] -> [b] -> Bool
subList [] _      = True
subList _ []      = False
subList is (x:xs) = check is (x:xs) || subList is xs
  where check [] _          = True
        check _ []          = False
        check (i:is) (x:xs) = i == x && check is xs