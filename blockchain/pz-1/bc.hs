import DataStructures (Address, TransactionType(..), Transaction(..), Block(..), Blockchain)

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
    , nonce = 0
    , previousHash = "0"
    , timeStamp = 0
}

chain :: Blockchain
chain = [genesisBlock]
