import qualified Data.Map as Map


helloPerson :: String -> String
helloPerson name = "Hi, " ++ name ++ "!"

main :: IO()
main = do
    putStrLn "What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement


userInput :: Map.Map Int String
userInput = Map.fromList [(1, "John")]

maybeMain :: Maybe String
maybeMain = do
    name <- Map.lookup 1 userInput
    let statement = helloPerson name
    return statement

