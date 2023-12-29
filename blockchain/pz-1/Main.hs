import DataStructures (Address, TransactionType(..), Transaction(..), Block(..), Blockchain)
import Helpers (blockHash, balance)

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
    print $ "alice balance: " ++ (show $ balance chain alice)
    print $ "bob balance: " ++ (show $ balance chain bob)

    let payForPizza = Transaction { author = alice
                                  , gas = 1
                                  , body = Transfer {to = bob, amount = 10000} }

    let pizzaCoinbase = Transaction { author = miner
                                    , gas = 0
                                    , body = Coinbase {reward = 25} }

    let pizzaBlock = Block { previousHash = blockHash genesisBlock
                           , timeStamp = 1
                           , nonce = 0
                           , transactions = [pizzaCoinbase, payForPizza] }

    -- todo use addBlock function instead
    let pizzaChain = chain ++ [pizzaBlock]
    print $ "alice balance: " ++ (show $ balance pizzaChain alice)