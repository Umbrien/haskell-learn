import Data.Semigroup
import Data.List

myLast :: [a] -> a
myLast = head . reverse

--myAll :: (a -> Bool) -> [a] -> Bool
--myAll tFunc = (foldr (&) True) . (map tFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny func = (foldr (||) True) . (map func)

l = [1,2,3]

data Color = Red | Yellow | Blue | Green | Purple
           | Orange | Brown | Clear deriving (Show, Eq)

instance Semigroup Color where
    (<>) Clear any = any
    (<>) any Clear = any
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b
          | a == b = a
          | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
          | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
          | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
          | otherwise = Brown

instance Monoid Color where
    mempty = Clear

myMax :: Ord a => [a] -> a
myMax = head . reverse . sort

