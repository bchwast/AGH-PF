doSafeTail3x :: [a] -> Maybe [a]
doSafeTail3x xs = do
  t1 <- safeTail xs
  t2 <- safeTail t1
  t3 <- safeTail t2
  return t3

safeTail3x :: [a] -> Maybe [a]
safeTail3x xs =
  safeTail xs >>= \t1 ->
    safeTail t1 >>= \t2 ->
      safeTail t2 >>= \t3 ->
        return t3

safeTail3x' :: [a] -> Maybe [a]
safeTail3x' xs = return xs >>= safeTail >>= safeTail >>= safeTail

f5 :: Int -> Int -> Int -> Int
f5 x y z = 1000 `div` x + 100 `div` y + 10 `div` z

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y | y /= 0    = Just $ x `div` y
            | otherwise = Nothing

safeF5 :: Int -> Int -> Int -> Maybe Int
safeF5 x y z =
  case (safeDiv 1000 x) of
    Nothing -> Nothing
    Just (iOverX) ->
      case (safeDiv 100 y) of
        Nothing -> Nothing
        Just (iOverY) ->
          case (safeDiv 10 z) of
            Nothing -> Nothing
            Just (iOverZ) -> Just $ iOverX + iOverY + iOverZ

safeF5' :: Int -> Int -> Int -> Maybe Int
safeF5' x y z = do
  iOverX <- safeDiv 1000 x
  iOverY <- safeDiv 100 y
  iOverZ <- safeDiv 10 z
  return $ iOverX + iOverY + iOverZ

safeF5'' :: Int -> Int -> Int -> Maybe Int
safeF5'' x y z = f <$> iOverX <*> iOverY <*> iOverZ
  where
    f i j k = i + j + k
    iOverX = safeDiv 1000 x
    iOverY = safeDiv 100 y
    iOverZ = safeDiv 10 z

sum10DivXi :: [Int] -> Int
sum10DivXi = foldr (\xi acc -> 10 `div` xi + acc) 0

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g = \x -> f x >>= g

maybeJoin :: Maybe (Maybe a) -> Maybe a
maybeJoin (Just (Just a)) = Just a
maybeJoin (Just Nothing)  = Nothing
maybeJoin Nothing         = Nothing

safeSum10DivXi :: [Int] -> Maybe Int
safeSum10DivXi []     = Just 0
safeSum10DivXi (x:xs) =
    case safeDiv 10 x of
        Nothing -> Nothing
        Just newX -> case safeSum10DivXi xs of
                            Nothing -> Nothing
                            Just newY -> Just (newX + newY)

safeSum10DivXi' :: [Int] -> Maybe Int
safeSum10DivXi' []     = Just 0
safeSum10DivXi' (x:xs) = do
    newX <- safeDiv 10 x
    newXs <- safeSum10DivXi' xs
    return $ newX + newXs

safeSum10DivXi'' :: [Int] -> Maybe Int
safeSum10DivXi'' []     = Just 0
safeSum10DivXi'' (x:xs) = safeDiv 10 x >>= \x -> safeSum10DivXi'' xs >>= \y -> return $ x + y

