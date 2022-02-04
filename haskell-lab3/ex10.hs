isSortedAcc :: Ord a => [a] -> Bool
isSortedAcc xs = and (zipWith (<=) xs (tail xs))

everySecond :: [t] -> [t]
everySecond xs = map fst $ filter (odd . snd) $ zip xs [1..length xs]

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _               = []
zip3' _ [] _               = []
zip3' _ _ []               = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

unzip3' :: [(a, b, c)] -> ([a], [b], [c])
unzip3' [] = ([], [], [])
unzip3' xs = (map (\(a, _, _) -> a) xs, map (\(_, b, _) -> b) xs, map (\(_, _, c) -> c) xs)

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = and (zipWith (>=) xs (tail xs))

isSorted :: Ord a => [a] -> Bool
isSorted xs = isSortedAsc xs || isSortedDesc xs