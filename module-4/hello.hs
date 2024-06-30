{-# LANGUAGE OverloadedStrings #-}

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

helloPerson :: String -> String
helloPerson name = "Hi, " ++ name ++ "!"

main :: IO ()
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

helloTextPerson :: T.Text -> T.Text
helloTextPerson name = "Hi, " <> name <> "!"

textMain :: IO ()
textMain = do
  TIO.putStrLn "What's your name?"
  name <- TIO.getLine
  let statement = helloTextPerson name
  TIO.putStrLn statement
