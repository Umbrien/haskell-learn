module Hash where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16

hash :: String -> String
hash input = hexhash
    where
        inputByteString = BC.pack input
        hashedByteString = SHA256.hash inputByteString
        hexhash = BC.unpack $ B16.encode hashedByteString