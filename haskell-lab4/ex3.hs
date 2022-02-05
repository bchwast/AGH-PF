data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT          = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT          = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a |
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n)    = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT        = 0
depthOfBT (Node n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT        = []
flattenBT (Node n lt rt) = flattenBT lt ++ [a] ++ flattenBT rt

flattenBT' :: BinTree a -> [a]
flattenBT' EmptyBT        = []
flattenBT' (Node n lt rt) = [a] ++ flattenBT' lt ++ flattenBT' rt

flattenBT'' :: BinTree a -> [a]
flattenBT'' EmptyBT        = []
flattenBT'' (Node n lt rt) = flattenBT'' rt ++ [a] ++ flattenBT'' lt

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT        = EmptyBT
mapBT f (Node n lt rt) = NodeBT (f a) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT        = NodeBT a EmptyBT EmptyBT
insert a (Node n lt rt)
  | a < n     = NodeBT n (insert a lt) rt
  | a == n    = NodeBT n lt rt
  | otherwise = NodeBT n lt (insert a rt)

occurs :: Eq a => a -> BinTree a -> Int
occurs a EmptyBT        = 0
occurs a (Node n lt rt)
  | a == n    = 1 + occurs a lt + occurs a rt
  | otherwise = occurs a lt + occurs a rt

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf _ EmptyBT        = False
elemOf a (Node n lt rt) = a == n || elemOf a lt || elemOf a rt

reflect :: BinTree a -> BinTree a
reflect EmptyBT        = EmptyBT
reflect (Node n lt rt) = NodeBT n (reflect rt) (reflect lt)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT                    = error "Empty tree"
minElemOf (NodeBT n EmptyBT EmptyBT) = n
minElemOf (NodeBT n lt EmptyBT)      = min n (minElemOf lt)
minElemOf (NodeBT n EmptyBT rt)      = min n (minElemOf rt)
minElemOf (NodeBT n lt rt)
  | n < minElemOf lt && n < minElemOf rt = n
  | otherwise                            = min (minElemOf lt) (minElemOf rt)

maxElemOf :: Ord a => BinTree a -> a
maxElemOf EmptyBT                    = error "Empty tree"
maxElemOf (NodeBT n EmptyBT EmptyBT) = n
maxElemOf (NodeBT n lt EmptyBT)      = max n (maxElemOf lt)
maxElemOf (NodeBT n EmptyBT rt)      = max n (maxElemOf rt)
maxElemOf (NodeBT n lt rt)
  | n > maxElemOf lt && n > maxElemOf rt = n
  | otherwise                            = max (maxElemOf lt) (maxElemOf rt)

list2BST :: Ord a => [a] -> BinTree a
list2BST []     = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBinTree _ a EmptyBT          = a
foldBinTree f a (NodeBT n lt rt) = f x (foldBinTree f a lt) (foldBinTree f a rt)

mapBT f = foldBinTree (\x lt rt -> NodeBT (f x) lt rt) EmptyBT

data GTree a = Leaf a |
               GNode [GTree a]
               deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf a)       = a
sumGTree (GNode [])     = 0
sumGTree (GNode (x:xs)) = sumGTree x + sumGTree (GNode xs)

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree x (Leaf a)       = a == x
elemOfGTree _ (GNode [])     = False
elemOfGTree a (GNode (x:xs)) = elemOfGTree a x || elemOfGTree a (GNode xs)

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf _)   = 1
depthOfGTree (GNode []) = 0
depthOfGTree (GNode (x:xs))
    | depthOfGTree x > depthOfGTree (GNode xs) = 1 + depthOfGTree x
    | otherwise                                = depthOfGTree (GNode xs)

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf x)   = Leaf $ f x
mapGTree f (GNode xs) = GNode (map (mapGTree f) xs)

flattenGTree :: GTree a -> [a]
flattenGTree (Leaf a)       = [a]
flattenGTree (GNode [])     = []
flattenGTree (GNode (x:xs)) = flattenGTree x ++ flattenGTree (GNode xs)

countGTreeLeaves :: GTree a -> Int
countGTreeLeaves (Leaf _)       = 1
countGTreeLeaves (GNode [])     = 0
countGTreeLeaves (GNode (x:xs)) = countGTreeLeaves x + countGTreeLeaves (GNode xs)

data Expr' a = Lit a |
               Add (Expr' a) (Expr' a) |
               Subtract (Expr' a) (Expr' a) |
               Multiply (Expr' a) (Expr' a)

eval' :: Num a => Expr' a -> a
eval' (Lit n)          = n
eval' (Add e1 e2)      = eval e1 + eval e2
eval' (Subtract e1 e2) = eval e1 - eval e2
eval' (Multiply e1 e2) = eval e1 * eval e2

show'' :: Show a => Expr' a -> String
show'' (Lit n)          = show n
show'' (Add e1 e2)      = "(" ++ show'' e1 ++ "+" ++ show'' e2 ++ ")"
show'' (Subtract e1 e2) = "(" ++ show'' e1 ++ "-" ++ show'' e2 ++ ")"
show'' (Multiply e1 e2) = "(" ++ show'' e1 ++ "*" ++ show'' e2 ++ ")"

data Expr'' a = Lit a |
                Expr'' a :+: Expr'' a |
                Expr'' a :-: Expr'' a |
                Expr'' a :*: Expr'' a

eval'' :: Num a => Expr'' a -> a
eval'' (Lit n)     = n
eval'' (e1 :+: e2) = eval e1 + eval e2
eval'' (e1 :-: e2) = eval e1 - eval e2
eval'' (e1 :*: e2) = eval e1 * eval e2

show''' :: Show a => Expr'' a -> String
show''' (Lit n)      = show n
show''' '(e1 :+: e2) = "(" ++ show''' e1 ++ "+" ++ show''' e2 ++ ")"
show''' (e1 :-: e2)  = "(" ++ show''' e1 ++ "-" ++ show''' e2 ++ ")"
show''' (e1 :*: e2)  = "(" ++ show''' e1 ++ "*" ++ show''' e2 ++ ")"