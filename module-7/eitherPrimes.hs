import Data.Char (isDigit)

primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show InvalidValue = "Numbers lower 2 are not checked"
  show TooLarge = "Max num to check is " ++ show maxN

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "Is prime number"
displayResult (Right False) = "Is NOT a prime number"
displayResult (Left primeError) = "Error: " ++ show primeError

main :: IO ()
main = do
  putStrLn "Enter num to check prime:"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)

addStrInts :: String -> String -> Either String Int
addStrInts a b
  | not (all isDigit a) = Left "First param is not Int"
  | not (all isDigit b) = Left "Second param is not Int"
  | all isDigit (a ++ b) = Right (read a + read b)

safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc a =
  if a == maxBound
    then Nothing
    else Just (succ a)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

safeLastBound :: Int
safeLastBound = 10

safeLast :: [a] -> Either String a
safeLast [] = Left "Empty list"
safeLast xs = safeLast' 10000 xs

safeLast' :: Int -> [a] -> Either String a
safeLast' 0 _ = Left "Too long list"
safeLast' _ [x] = Right x -- [x] is same as (x:[])
safeLast' n (x : xs) = safeLast' (n - 1) xs