import Data.Char (isPunctuation, isSpace)
import Data.Text as T
import Data.Text.IO as TIO
import Lib (isPalindrome, preprocess)
import Test.QuickCheck (Args (maxSuccess), quickCheck, quickCheckWith, stdArgs)
-- quickcheck-instances
import Test.QuickCheck.Instances ()

-- Invariant property
-- Call of preprocess should return same value as call on the same value w/o punctuation marks
prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_whiteSpaceInvariant :: T.Text -> Bool
prop_whiteSpaceInvariant text = isPalindrome text == isPalindrome textWithoutSpaces
  where
    textWithoutSpaces = T.filter (not . isSpace) text

prop_uppercaseInvariant :: T.Text -> Bool
prop_uppercaseInvariant text = preprocess text == preprocess (T.toUpper text)

assert :: Bool -> T.Text -> T.Text -> IO ()
assert test passStatement failStatement =
  if test
    then TIO.putStrLn passStatement
    else TIO.putStrLn failStatement

assertIsPalindrome :: (T.Text -> Bool) -> T.Text -> IO ()
assertIsPalindrome f str = assert (f str) successStatement failStatement
  where
    successStatement :: T.Text
    successStatement = mconcat ["passed: `", str, "`"]
    failStatement :: T.Text
    failStatement = mconcat ["failed: `", str, "`"]

main :: IO ()
main = do
  TIO.putStrLn "Starting testing suite..."
  assertIsPalindrome isPalindrome "dohod"
  assertIsPalindrome isPalindrome "dohod!"
  assertIsPalindrome isPalindrome "dohod."
  assertIsPalindrome isPalindrome ":dohod:"
  assertIsPalindrome (not . isPalindrome) "cat"
  assertIsPalindrome (not . isPalindrome) "cat!"
  TIO.putStrLn "--- QuickCheck ---"
  quickCheck prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheck prop_whiteSpaceInvariant -- gives success on 100 tests if program contains bug
  quickCheckWith stdArgs {maxSuccess = 1000} prop_whiteSpaceInvariant
  quickCheck prop_uppercaseInvariant
  TIO.putStrLn "Ready"
