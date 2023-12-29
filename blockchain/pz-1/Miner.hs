module Miner where

import DataStructures (Block(..), Transaction(..), TransactionType(..), Address)
import Helpers (coinbaseReward, blockHash, hashDifficultyValid)
import Hash (Hash)

findNonce :: Hash -> [Transaction] -> Int
findNonce previousHash transactions = head $ filter (\nonce -> hashDifficultyValid $ blockHash $ newBlock nonce) [0..]
  where
    newBlock :: Int -> Block
    newBlock nonce = Block { previousHash = previousHash
                           , timeStamp = 0
                           , nonce = nonce
                           , transactions = transactions }

coinbaseTransaction :: Address -> [Transaction] -> Transaction
coinbaseTransaction minerAddress transactions =
    Transaction { author = minerAddress
                , gas = 0
                , body = Coinbase { reward = coinbaseReward transactions } }

createBlock :: Hash -> Address -> [Transaction] -> Block
createBlock previousHash minerAddress transactions = newBlock
  where
    transactionsWithCoinbase = [coinbaseTransaction minerAddress transactions] ++ transactions
    nonce = findNonce previousHash transactionsWithCoinbase
    newBlock = Block { previousHash = previousHash
                     , timeStamp = 0
                     , nonce = nonce
                     , transactions = transactionsWithCoinbase }