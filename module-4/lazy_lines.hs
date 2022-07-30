sampleData :: [Char]
sampleData = ['6','2','\n','2','1','\n']

toInts :: [Char] -> [Int]
toInts = map read . lines

toSquares :: [Int] -> [Int]
toSquares = map (^2)

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = toSquares numbers
  print $ sum squares

