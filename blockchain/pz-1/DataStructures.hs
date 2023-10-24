module DataStructures where

type Address = String

data TransactionType = Transfer {
    to :: Address
    , amount :: Float
} | SmartContractCall {
    contract :: Address
    , meta :: String
} deriving Show

data Transaction = Transaction {
    author :: Address
    , gas :: Float
    , body :: TransactionType
} deriving Show

data Block = Block {
    index :: Int
    , transactions :: [Transaction]
    , nonce :: Int
    , previousHash :: String
    , timeStamp :: Int
} deriving Show

type Blockchain = [Block]

difficulty :: Int
difficulty = 1
