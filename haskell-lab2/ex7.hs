sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs