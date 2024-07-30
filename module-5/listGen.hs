import Control.Monad (guard)
import Data.Char (toUpper)

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powersOfTwo' :: Int -> [Int]
powersOfTwo' n = [2 ^ value | value <- [1 .. n]]

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (2 ^) [1 .. n]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

powersOfTwoAndThree' :: Int -> [(Int, Int)]
powersOfTwoAndThree' n =
  [ (powersOfTwo, powersOfThree)
    | value <- [1 .. n],
      let powersOfTwo = 2 ^ value,
      let powersOfThree = 3 ^ value
  ]

powersOfTwoAndThreeMap :: Int -> [(Int, Int)]
powersOfTwoAndThreeMap n = map (\x -> (2 ^ x, 3 ^ x)) [1 .. n]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)

allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n =
  [ (evenValue, oddValue)
    | evenValue <- [2, 4 .. n],
      oddValue <- [1, 3 .. n]
  ]

squaresPairsTo10 :: [(Int, Int)]
squaresPairsTo10 = do
  values <- [1 .. 10]
  let squares = 2 ^ values
  return (values, squares)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

evensGuard' :: Int -> [Int]
evensGuard' n = [value | value <- [1 .. n], even value]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f list = do
  vals <- list
  guard (f vals)
  return vals

filtered :: [Int]
filtered = myFilter (> 3) [1 .. 10]

colors :: [String]
colors = ["brown", "blue", "pink", "orange"]

misters :: [String] -> [String]
misters colors =
  [ mister
    | (x : xs) <- colors,
      let mister = "Mr. " ++ toUpper x : xs
  ]

monthDates :: String -> Int -> [String]
monthDates month days =
  [ date
    | day <- [1 .. days],
      let date = month ++ " " ++ show day
  ]

calendarDates :: [String]
calendarDates = janDays ++ febDays
  where
    janDays = monthDates "Jan" 30
    febDays = monthDates "Feb" 29

monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [date | end <- ends, date <- [1 .. end]]

datesDo :: [Int] -> [Int]
datesDo ends = do
  end <- ends
  date <- [1 .. end]
  return date

-- [1..end]

daysM :: [Int] -> [Int]
daysM ends = ends >>= \end -> [1 .. end] >>= \date -> return date