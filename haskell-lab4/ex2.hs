-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefiks 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue | White | Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec a _ _) = a

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' {x::a, y::a, z::a}

polarToCartesian :: Floating a => Cart2DVec'' a -> Cart2DVec'' a
polarToCartesian Cart2DVec'' {x = r, y = phi} = cart2DVec'' {x = r * cos phi, y= r * sin phi}

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle a)      = pi * a^2
area (Rectangle a b) = a * b

data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving Show

rootValue :: Tree a -> a
rootValue EmptyT       = error "rootValue: tree is empty"
rootValue (Node a _ _) = a

data TrafficLights = Red | Yellow | Green

actionFor :: TrafficLights -> String
actionFor Red    = "Stop"
actionFor Yellow = "Get ready"
actionFor Green  = "Go"

data DriverAction = Stop | Ready | Go

driverActionFor :: TrafficLights -> DriverAction
driverActionFor Red    = Stop
driverActionFor Yellow = Ready
drivarActionFor Green  = Go