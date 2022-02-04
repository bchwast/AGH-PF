import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc' xs = reverse (sort xs)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g xs = [f x | x <- xs] == [g x | x <- xs]

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

composeFunList :: (a -> a) -> (a -> a)
composeFunList []     = id
composeFunList (x:xs) = x . composeFunList xs