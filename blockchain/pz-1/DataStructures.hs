module DataStructures where

import Hash (Hash)

type Address = String

data TransactionType =
    Coinbase { reward :: Float }
    | Transfer { to :: Address , amount :: Float }
    | SmartContractCall { contract :: Address , meta :: String }

data Transaction = Transaction { author :: Address
                               , gas :: Float
                               , body :: TransactionType }

data Block = Block { previousHash :: Hash
                   , timeStamp :: Int
                   , nonce :: Int
                   , transactions :: [Transaction] }

type Blockchain = [Block]

instance Show TransactionType where
    show (Transfer to amount) = "Transfer " ++ show amount ++ " to " ++ show to
    show (SmartContractCall contract meta) = "Contract call " ++ show contract ++ " │ meta " ++ show meta
    show (Coinbase reward) = "Coinbase reward " ++ show reward

instance Show Transaction where
    show (Transaction author gas body) =
        show body ++ " │ gas " ++ show gas ++ " │ author " ++ show author

instance Show Block where
    show (Block prevHash timeStamp nonce transactions) =
        "\n│ Block (prevHash " ++ show prevHash ++ ") at " ++ show timeStamp ++ " │ nonce " ++ show nonce ++
        concatMap (\t -> "\n│ │ " ++ show t) transactions ++ "\n│"


difficulty :: Int
difficulty = 3

blockReward :: Float
blockReward = 25
