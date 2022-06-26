data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
type FullName = Triple String
type Initials = Triple Char

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

aPerson :: FullName
aPerson = Triple "Hovard" "Phillips" "Lovecraft"

initials :: Initials
initials = Triple 'H' 'P' 'L'

data Bable a = Bable a a deriving Show
