concat' :: [[a]] -> [a]
concat' []    = []
concat' (x:xs) = x ++ concat' xs

concat'' xs = [i | x <- xs, i <- x]

concat''' xs = foldr (++)