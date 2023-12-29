module DataStructures where

import Hash (Hash)

type Address = String

data TransactionType =
    Transfer { to :: Address , amount :: Float }
    | SmartContractCall { contract :: Address , meta :: String }
    | Coinbase { reward :: Float } deriving Show

data Transaction = Transaction { author :: Address
                               , gas :: Float
                               , body :: TransactionType } deriving Show

data Block = Block { previousHash :: Hash
                   , timeStamp :: Int
                   , nonce :: Int
                   , transactions :: [Transaction] } deriving Show

type Blockchain = [Block]

difficulty :: Int
difficulty = 1

coinbaseReward :: Float
coinbaseReward = 25
