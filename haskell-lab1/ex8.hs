not' :: Bool -> Bool
not' b = case b of
          True -> False
          False -> True

absInt :: Int -> Int
absInt n =
  case (n >= 0) of
    True -> n
    _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer ans =
  case (ans == "Love") of
    True -> True
    _    -> False

or' :: (Bool, Bool) -> Bool
or' (a, b) =
  case (a || b) of
    True  -> True
    False -> False

and' :: (Bool, Bool) -> Bool
and' (a, b) =
  case (a && b) of
    True  -> True
    False -> False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) =
  case (a && b) of
    True  -> False
    False -> True

xor' :: (Bool, Bool) -> Bool
xor' (a, b) =
  case ((a && b) || not (a || b)) of
    True  -> False
    False -> True