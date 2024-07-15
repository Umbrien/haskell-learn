minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree a b c = min a $ min b c

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
  putStrLn "Enter 3 nums"
  minInt <- minOfInts
  putStrLn $ show minInt ++ " is min"

minMaybe :: Maybe Int
minMaybe = minOfThree <$> Just 10 <*> Just 3 <*> Just 6