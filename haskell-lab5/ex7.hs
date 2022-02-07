newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor (Either e) where
  fmap g (Left e)  = Left e
  fmap g (Right x) = Right (g x)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
    fmap g (MyTriple (a,b,c)) = MyTriple (g a, g b, g c)

instance Applicative MyTriple where
    pure a = MyTriple (a,a,a)
    MyTriple (f,g,h) <*> MyTriple (a,b,c) = MyTriple (f a, g b, h c)