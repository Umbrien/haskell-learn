{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (appendFile, readFile)

type Stats = (Int, Int, Int)

getCounts :: T.Text -> Stats
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = (length . T.words) input
    lineCount = (length . T.lines) input

countsText :: Stats -> T.Text
countsText (cc, wc, lc) = T.pack $ unwords ["symbols:", show cc, "words:", show wc, "lines:", show lc]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = (countsText . getCounts) input
  TIO.appendFile "stats.dat" $ mconcat [T.pack fileName, " ", summary, "\n"]
  TIO.putStrLn summary
