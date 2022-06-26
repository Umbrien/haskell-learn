-- Hello flusq in Haskell

--main :: IO ()
--main = putStrLn "Hello lutierre"


getTitle title = "Hello, " ++ title ++ "!\n"
getBody body = "Thanks for buying " ++ body ++ "!"

getEmail title body = getTitle title ++
                      getBody body

mathFunc n = if even n
             then 2 - n
             else 3 * n + 2

main = do
  --putStrLn "Hello lutierre!"
  --putStrLn "Enter name: "
  --name <- getLine
  --putStrLn "Enter book name: "
  --book <- getLine
  --putStrLn (getEmail name book)

  putStrLn (show (mathFunc 1))



