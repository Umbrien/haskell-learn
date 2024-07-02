{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

bsInt :: B.ByteString
bsInt = "4"

bsToInt :: B.ByteString -> Int
bsToInt = read . BC.unpack
