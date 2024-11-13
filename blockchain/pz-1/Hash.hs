module Hash where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16

type Hash = String

hash :: String -> Hash
hash = BC.unpack . B16.encode . SHA256.hash . BC.pack
