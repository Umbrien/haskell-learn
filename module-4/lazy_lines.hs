{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO

sampleData :: [Char]
sampleData = ['6', '2', '\n', '2', '1', '\n']

toInts :: [Char] -> [Int]
toInts = map read . lines

toSquares :: [Int] -> [Int]
toSquares = map (^ 2)

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = toSquares numbers
  print $ sum squares

textToInts :: T.Text -> [Int]
textToInts = map (read . T.unpack) . T.lines

textMain :: IO ()
textMain = do
  userInput <- TIO.getContents
  let numbers = textToInts userInput
  TIO.putStrLn $ (T.pack . show . sum) numbers
