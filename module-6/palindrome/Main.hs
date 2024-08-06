module Main where

import Palindrome (isPalindrome)

main :: IO ()
main = do
  putStrLn "Enter the word:"
  text <- getLine
  let response =
        if isPalindrome text
          then "This is palindrome"
          else "This is not palindrome"
  putStrLn response