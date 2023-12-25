import DataStructures (Address, TransactionType(..), Transaction(..), Block(..), Blockchain)
import Helpers (blockHash)

alice :: Address
alice = "alice.near"

bob :: Address
bob = "bob.eth"

miner :: Address
miner = "miner"

burn :: Address
burn = "0x0000000000000000000000000000000000000000"

topUpAlice :: Transaction
topUpAlice = Transaction { author = burn
                         , gas = 0
                         , body = Transfer {to = alice, amount = 100000} }

topUpBob :: Transaction
topUpBob = Transaction { author = burn
                       , gas = 0
                       , body = Transfer {to = bob, amount = 100} }

genesisBlock :: Block
genesisBlock = Block { previousHash = "0"
                     , timeStamp = 0
                     , nonce = 0
                     , transactions = [topUpAlice, topUpBob] }

chain :: Blockchain
chain = [genesisBlock]

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

    let payForPizza = Transaction { author = alice
                                  , gas = 1
                                  , body = Transfer {to = bob, amount = 10000} }

    let pizzaBlock = Block { previousHash = blockHash genesisBlock
                           , timeStamp = 0
                           , nonce = 0
                           , transactions = [payForPizza] }

    print pizzaBlock
