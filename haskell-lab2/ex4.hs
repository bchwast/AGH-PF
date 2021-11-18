isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: [a] -> Int -> [a]
getElemAtIdx s i
    | i == 0         = [head s]
    | i == length s  = [last s]
    | otherwise      = drop (i - 1) (take i s)

toUpper :: Char -> Char
toUpper a = if fromEnum a > 90
            then toEnum (fromEnum a - 32)
            else a

capitalize :: [Char] -> [Char]
capitalize s
      | s == []   = []
      | otherwise = [toUpper (head s)] ++ capitalize (tail s)