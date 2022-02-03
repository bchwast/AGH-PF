absInt :: Int -> Int
absInt n | n >= 0 = n
         | otherwise = -n

sgn :: Int -> Int
sgn n | n < 0 = -1
      | n == 0 = 0
      | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) | a <= b && a <= c = a
                  | b <= a && b <= c = b
                  | c <= a && c <= b = c

toLower :: Char -> Char
toLower a | fromEnum a > 64 && fromEnum a < 91 = toEnum (fromEnum a + 32)
          | otherwise                          = a

toUpper :: Char -> Char
toUpper a | fromEnum a > 96 && fromEnum a < 123 = toEnum (fromEnum a - 32)
          | otherwise                           = a

isDigit :: Char -> Bool
isDigit a | 47 < fromEnum a < 58 = True
          | otherwise            = False

charToNum :: Char -> Int
charToNum a | isDigit a = fromEnum a - 48
            | otherwise = 0

romanDigit :: Char -> String
romanDigit a | a == '1'  = "I"
             | a == '2'  = "II"
             | a == '3'  = "III"
             | a == '4'  = "IV"
             | a == '5'  = "V"
             | a == '6'  = "VI"
             | a == '7'  = "VII"
             | a == '8'  = "VIII"
             | a == '9'  = "IX"
             | otherwise = ""