polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)
polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a)
newtype PolarCoord'' a = MkPolarCoord'' (a, a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = (MkCartesianCoord'' (r * cos phi, r * sin phi))

personInfoToString :: (String, String, String) -> String
personInfoToString (nm, snm, addr) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", address: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType
personInfoToString' (nm, snm, addr) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", address: " ++ addr

newtype Name'' = MkName'' String
newtype Surname'' = MkSurname'' String
newtype Address'' = MkAddress'' String
newtype PersonInfo'' = MkPersonInfo'' (Name'', Surname'', Address'')

personInfoToString'' :: PersonInfo'' -> String
personInfoToStrin'' (MkPersonInfo'' (MkName'' name, MkSurname'' surname, MkAddress'' address)) =
  "name: " ++ name ++ ", surname: " ++ surname ++ ", address: " ++ address
