module Miner where

import Hash (Hash)
import DataStructures (Block(..))
import Helpers (blockHash)


blockHashByNonce :: Block -> Int -> Hash
blockHashByNonce (Block index transactions _ previousHash timeStamp) n = blockHash Block {
    index = index
    , transactions = transactions
    , nonce = n
    , previousHash = previousHash
    , timeStamp = timeStamp
}