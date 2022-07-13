newtype Box a = Box a deriving Show

wrap :: a -> Box a
wrap = Box
--wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving Show

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

type Point3D = Triple Double
type FullName = Triple String
type Initials = Triple Char

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

aPerson :: FullName
aPerson = Triple "Hovard" "Phillips" "Lovecraft"

initials :: Initials
initials = Triple 'H' 'P' 'L'



---



--data List a = Empty | Cons a (List a) deriving Show
--ourIntList = Cons 1 $ Cons 2 $ Cons 3 Empty

infixr 5 :::
data List a = Empty | a ::: (List a) deriving Show

ourIntList :: List Int
ourIntList = 1 ::: 2 ::: 3 ::: Empty

ourCharList :: List Char
ourCharList = 'c' ::: 'a' ::: 't' ::: Empty

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (a ::: rest) = f a ::: ourMap f rest

