{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show, Functor)

instance Functor BinTree where
  fmap _ EmptyBT                              = EmptyBT
  fmap f (NodeBT x lt rt) = NodeBT (f x) (fmap f lt) (fmap f rt)

newtype Pair b a = Pair { getPair :: (a,b) } deriving Show
-- fmap should change the first element

instance Functor (Pair b) where
    fmap g Pair { getPair = (a,b) } = Pair {getPair = (g a,b)}

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where
    fmap g EmptyT2 = EmptyT2
    fmap g (Leaf a) = Leaf (g a)
    fmap g (Node lt a rt) = Node (fmap g lt) (g a) (fmap g rt)

data GTree a = Leaf a | GNode [GTree a] deriving Show

instance Functor GTree where
    fmap g (Leaf a) = Leaf (g a)
    fmap g (GNode xs) = GNode (map (fmap g) xs)