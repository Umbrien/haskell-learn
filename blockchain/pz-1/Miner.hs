module Miner where

import Hash (Hash)
import DataStructures (Block(..))
import Helpers (blockHash)

testNonce :: Int -> String
testNonce nonce = blockHash Block { previousHash = "0"
                                  , timeStamp = 0
                                  , nonce = nonce
                                  , transactions = [] }
