import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.List (intercalate)


data Organ = Heart | Brain | Kidney | Spleen
  deriving (Eq, Ord, Show)

--instance Show Organ where
--    show Heart = "Сердце"
--    show Brain = "Мозг"
--    show Kidney = "Почка"
--    show Spleen = "Селезёнка"

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> Map.lookup id catalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter isNothing
--emptyDrawers [] = 0
--emptyDrawers (x:xs) = num + emptyDrawers xs
--  where num = if isNothing x then 1 else 0

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
--countOrgan :: [Maybe Organ] -> Organ -> Int
--countOrgan organs organ = length $ filter (== Just organ) organs
--countOrgan organ organs = length $ filter (== Just organ) organs
countOrgan organ = length . filter (== Just organ)

--countAvailableOrgan = countOrgan availableOrgans

isSomething :: Maybe a -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just n) = n


----


data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in vat"
  show (Cooler organ) = show organ ++ " in cooler"
  show (Bag organ) = show organ ++ " in bag"

data Location = Lab | Kitchen | Bathroom
  deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (location, container) =
    show container ++
    " (place: " ++
    show location ++ ")"

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report $ process organ
processAndReport Nothing = "Error gang shit like shimmer"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog


---
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just val) = Just (f val)

-- compiles but crashes with next usage:
-- maybeMap (+2) [(Just 2), (Just 3)]
-- Non-exhastive patterns in function maybeMap
-----
--maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
--maybeMap f (Nothing:xs) = Nothing : maybeMap f xs
--maybeMap f ((Just val):xs) = Just (f val) : maybeMap f xs

