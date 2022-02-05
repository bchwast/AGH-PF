data MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromIntegral int              = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show

instance Eq a => Eq (BinTree a) where
    (==) EmptyBT EmptyBT                       = True
    (==) _ EmptyBT                             = False
    (==) EmptyBT _                             = False
    (==) (NodeBT a alt art) (NodeBT b blt brt) = a == b && alt == blt && art == brt

data Cart3DVec a = Cart3DVec a a a

instance Eq a => Eq (Cart3DVec a) where
    (==) (Cart3DVec a b c) (Cart3DVec x y z) = a == x && b == y && c == z

instance Num a => Num (Cart3DVec a) where
    (+) (Cart3DVec a b c) (Cart3DVec x y z) = Cart3DVec (a + x) (b + y) (c + z)
    (-) (Cart3DVec a b c) (Cart3DVec x y z) = Cart3DVec (a - x) (b - y) (c - z)
    (*) (Cart3DVec a b c) (Cart3DVec x y z) = Cart3DVec (a * x) (b * y) (c * z)

instance Ord a => Ord (Cart3DVec a) where
    (<) (Cart3DVec a b c) (Cart3DVec x y z)
        | a == x && b == y = c < z
        | a == x           = b < y
        | otherwise        = a < x