module Miner where

import DataStructures (Block(..), Transaction(..), TransactionType(..), Address)
import Helpers (coinbaseReward, blockHash)

testNonce :: Int -> String
testNonce nonce = blockHash Block { previousHash = "0"
                                  , timeStamp = 0
                                  , nonce = nonce
                                  , transactions = [] }

coinbaseTransaction :: Address -> [Transaction] -> Transaction
coinbaseTransaction minerAddress transactions =
    Transaction { author = minerAddress
                , gas = 0
                , body = Coinbase { reward = coinbaseReward transactions } }
