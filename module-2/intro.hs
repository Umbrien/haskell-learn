average aList = fromIntegral (sum aList) / fromIntegral (length aList)

halve :: Int -> Int
halve x = x `div` 2

printDouble :: Int -> String
printDouble x = show (x * 2)

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)


