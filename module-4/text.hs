{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T -- or Data.Text.Lazy
import Data.Semigroup

firstWord :: String
firstWord = "miui"

-- pack & unpack are expensive operations
secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "pixel experience"

main :: IO ()
main = do
    print aWord

sampleInput :: T.Text
sampleInput = "some\nsample\ninput"

-- lines but for T.Text
lined :: [T.Text]
lined = T.lines sampleInput

unlined :: T.Text
unlined = T.unlines lined

otherText :: T.Text
otherText = "some\ncompletely different\t text"

-- same as lines but not only \n but all spacing characters
textWords :: [T.Text]
textWords = T.words otherText

unTextWords :: T.Text
unTextWords = T.unwords textWords

breakText :: T.Text
breakText = "easy"

textToSplit :: T.Text
textToSplit = "It's easy to do"

splitten :: [T.Text]
splitten = T.splitOn breakText textToSplit

intercalated :: T.Text
intercalated = T.intercalate breakText splitten

myLines :: T.Text -> [T.Text]
myLines = T.splitOn "\n"

myUnlines :: [T.Text] -> T.Text
myUnlines = T.intercalate "\n"

-- can't do ++ with Text since it's not list
combinedStrings :: String
combinedStrings = "a" ++ "b"

-- but here are 2 alternatives

-- don't need import Data.Semigroup for it
combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["a", "b"]

-- need import Data.Semigroup for it
combinedTextSemigroup :: T.Text
combinedTextSemigroup = "a" <> "b"

