sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
             then 0
             else 1

absInt :: Int -> Int
absInt n = if n < 0
           then (-1) * n
           else n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a > b
                 then b
                 else a

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if a > b
                    then
                        if b > c
                        then c
                        else b
                    else if a > c
                        then c
                        else a

min3IntMin2 :: (Int, Int, Int) -> Int
min3IntMin2 (a, b, c) = if a < min2Int (b, c)
                    then a
                    else min2Int (b, c)

toUpper :: Char -> Char
toUpper a = if fromEnum a > 96 && fromEnum a < 123
            then toEnum (fromEnum a - 32)
            else a

toLower :: Char -> Char
toLower a = if fromEnum a > 64 && fromEnum a < 91
            then toEnum (fromEnum a + 32)
            else a

isDigit :: Char -> Bool
isDigit a = if 47 < fromEnum a && fromEnum a < 58
            then True
            else False

charToNum :: Char -> Int
charToNum a = if isDigit a
              then fromEnum a - 48
              else 0

romanDigit :: Char -> String
romanDigit a = if fromEnum a == 49
               then "I"
               else if fromEnum a == 50
                    then "II"
                    else if fromEnum a == 51
                         then "III"
                         else if fromEnum a == 52
                              then "IV"
                              else if fromEnum a == 53
                                   then "V"
                                   else if fromEnum a == 54
                                        then "VI"
                                        else if fromEnum a == 55
                                             then "VII"
                                             else if fromEnum a == 56
                                                  then "VIII"
                                                  else if fromEnum a == 57
                                                       then "IX"
                                                       else ""