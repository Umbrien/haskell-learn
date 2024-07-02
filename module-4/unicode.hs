{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO

dharmaText :: T.Text
dharmaText = "धर्म"

dharmaSafe :: B.ByteString
dharmaSafe = E.encodeUtf8 dharmaText

main :: IO ()
main = do
  TIO.putStrLn $ E.decodeUtf8 dharmaSafe
