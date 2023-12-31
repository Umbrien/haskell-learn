import DataStructures (Address, TransactionType(..), Transaction(..), Block(..), Blockchain)
import Helpers (blockHash, balance, addBlock)
import Miner (createBlock)

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
                     , nonce = 29459
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

    let callCastledice = Transaction { author = alice
                                     , gas = 100
                                     , body = SmartContractCall {contract = "castledice.near", meta = "roll 100"} }

    let pizzaBlock = createBlock (blockHash genesisBlock) miner [payForPizza, callCastledice]
    print pizzaBlock
    print $ blockHash pizzaBlock

    let pizzaChain = addBlock chain [pizzaBlock]
    print pizzaChain
    print $ "alice balance: " ++ (show $ balance pizzaChain alice)
    print $ "bob balance: " ++ (show $ balance pizzaChain bob)
    print $ "miner balance: " ++ (show $ balance pizzaChain miner)
