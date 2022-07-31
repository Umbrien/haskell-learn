main :: IO ()
main = do
  userInput <- getContents
  let isPalims = map (\x -> x == reverse x) $ lines userInput
  mapM_ print isPalims

