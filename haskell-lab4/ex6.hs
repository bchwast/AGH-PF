class Mappable t where
  fMap :: (a -> b) -> t a -> t b

data Vec3D a = Vec3D {x::a, y::a, z::a} deriving Show

instance Mappable Vec3D where
  fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

newtype Pair a = Pair (a, a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x, y)) = Pair (f x, f y)

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
                 deriving Show
instance Mappable BinTree where
    fMap _ EmptyBT = EmptyBT
    fMap f (NodeBT a left right) = NodeBT (f a) (fMap f left) (fMap f right)

instance Mappable Maybe where
    fMap _ Nothing = Nothing
    fMap f (Just a) = Just (f a)

instance Mappable (Either a) where
     fMap f (Left x) = Left x
     fMap f (Right y) = Right (f y)

instance Mappable ((->) a) where
  fMap f g = f.g

class VectorLike t where
 (|==|) :: Eq a => t a -> t a -> Bool
 (|+|), (|-|) :: (Num a) => t a -> t a -> t a
 (|*|) :: (Num a) => t a -> t a -> a
 (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
 vectLength :: Floating a => t a -> a
 unitVectOf :: Floating a => t a -> t a

data Vec2D a = Vec2D a a deriving Show

instance VectorLike Vec2D where
    (|==|) (Vec2D x1 y1) (Vec2D x2 y2) = x1 == x2 && y1 == y2
    (|+|) (Vec2D x1 y1) (Vec2D x2 y2)  = Vec2D (x1 + x2) (y1 + y2)
    (|-|) (Vec2D x1 y1) (Vec2D x2 y2)  = Vec2D (x1 - x2) (y1 - y2)
    (|*|) (Vec2D x1 y1) (Vec2D x2 y2)  = (x1 * x2) + (y1 * y2)
    (||?) (Vec2D x1 y1) (Vec2D x2 y2)  = (x1 * y2) - (x2 * y1) == 0
    (|-?) (Vec2D x1 y1) (Vec2D x2 y2)  = (x1 * x2) + (y1 * y2) == 0
    vectLength (Vec2D x1 y1)           = sqrt (x1^2 + y1^2)
    unitVectOf (Vec2D x1 y1)           = Vec2D (vl/x1) (vl/y1)
        where vl = vectLength (Vec2D x1 y1)
  