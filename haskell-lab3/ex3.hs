sqr x = x^2

funcFactory n = case n of
  1 -> id
  2 -> sqr
  3 -> (^3)
  4 -> \x -> x^4
  5 -> intFunc
  _ -> const n
  where
    intFunc x = x^5

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f x) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x + h) - f (x - h)) / (2 * h)
