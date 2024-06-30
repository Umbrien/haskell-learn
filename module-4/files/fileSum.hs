fileName = "nums.txt"

main :: IO ()
main = do
  inputFile <- readFile fileName
  let nums :: [Int] = map read $ lines inputFile
  print $ sum nums
