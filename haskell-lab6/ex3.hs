safeTail :: [a] -> Either String [a]
safeTail []     = Left "Empty list!"
safeTail (x:xs) = Right xs

doSafeTail3x :: [a] -> Either String [a]
doSafeTail3x xs = do
  t1 <- safeTail xs
  t2 <- safeTail t1
  t3 <- safeTail t2
  return t3

safeDiv :: Int -> Int -> Either String Int
safeDiv x y | y /= 0    = Right $ x `div` y
            | otherwise = Left "Cannot div by zero!"

safeF5 :: Int -> Int -> Int -> Either String Int
safeF5 x y z =
  case (safeDiv 1000 x) of
    Left e -> Left e
    Right (iOverX) ->
      case (safeDiv 100 y) of
        Left e -> Left e
        Right (iOverY) ->
          case (safeDiv 10 z) of
            Left e -> Left e
            Right (iOverZ) -> Right $ iOverX + iOverY + iOverZ

safeF5' :: Int -> Int -> Int -> Either String Int
safeF5' x y z = do
  iOverX <- safeDiv 1000 x
  iOverY <- safeDiv 100 y
  iOverZ <- safeDiv 10 z
  return $ iOverX + iOverY + iOverZ

eitherJoin :: Either a (Either a b) -> Either a b
eitherJoin (Right (Left a)) = Left a
eitherJoin (Right (Right a)) = Right a
eitherJoin (Left a) = Left a

safeDiv' :: Int -> Int -> Either String Int
safeDiv' x y | y /= 0    = Right $ x `div` y
            | otherwise = Left "Cannot div by zero!"

safeF5'' :: [Int] -> Either String Int
safeF5'' [] = Right 0
safeF5'' (x:xs) = safeDiv' 10 x >>= \x -> safeF5'' xs >>= \y -> return $ x + y

