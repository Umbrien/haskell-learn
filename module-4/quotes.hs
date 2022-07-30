quotes :: [String]
quotes = ["q1", "q2", "q3", "q4", "q5"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
  where quote = quotes !! (read x - 1)

main :: IO ()
main = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote $ lines userInput)


