import Hash (hash)

--- --- --- Data structures

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
    , proof :: Int
    , previousHash :: String
    , timeStamp :: Int
} deriving Show

type Blockchain = [Block]

--- --- --- Helper functions

blockHash :: Block -> String
blockHash b = hash serialized
    where
        serialized = "serialized"

-- todo check if transaction is valid:
-- - sender balance is enough for gas + (if Transaction is Transfer, then + amount)
-- - if multiple transactions from one sender in the same block, make sure it will work as should: user cannot fool system and use his balance twice
-- - last miner Transaction is Transfer with amount equal to all fees
isBlockValid :: ([Block], Block) -> Bool
isBlockValid ([], _) = True -- or not just True if other uslovia below?
isBlockValid (blockchain, block) = and [isProofUnique, isHashValid] -- todo more uslovia??
    where
        existingProofs = map proof blockchain
        isProofUnique = all (\a -> not $ a == proof block) existingProofs
        isHashValid = (previousHash block) == blockHash (last blockchain)
        -- isMinerTransactionValid = and [ ]

addBlock :: Blockchain -> [Block] -> Blockchain
addBlock blockchain blocks = blockchain ++ [approvedBlock]
    where
        validBlocks = filter (\bs -> isBlockValid (blockchain, bs)) blocks
        greatestProof = foldl (\acc curr -> max acc (proof curr)) 0 blocks
        approvedBlock = head $ filter (\b -> proof b == greatestProof) validBlocks -- will cause exception if filter resuts in []

--- --- --- Client code

alice :: Address
alice = "alice.near"

bob :: Address
bob = "bob.eth"

burn :: Address
burn = "0x0000000000000000000000000000000000000000"

topUpAlice :: Transaction
topUpAlice = Transaction { author = burn, gas = 0.00, body = Transfer {to = alice, amount = 10} }

topUpBob :: Transaction
topUpBob = Transaction { author = burn, gas = 0.00, body = Transfer {to = bob, amount = 100} }

genesisBlock :: Block
genesisBlock = Block {
    index = 0
    , transactions = [topUpAlice, topUpBob]
    , proof = 0
    , previousHash = "0"
    , timeStamp = 0
}

chain :: Blockchain
chain = [genesisBlock]
