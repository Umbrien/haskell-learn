## Assumptions

- As of development time, there is single-node architecture. I hope it will change to decentralised

- No transaction signatures

- Proof of Work realisation: nonce is proof: unique number, which if added to block contents, will give beautiful hash (starting with n 0s)

- Genesis block generates some balance out of nowhere for demo purposes
- Genesis block is considered valid, validating occurs to blocks that come after it

# What already exists

- Coinbase validation: check if it is the first transaction in a block and there is only one coinbase transaction in a block

## How it will work for user

- There is transaction pool from which miners will pick transactions they want to include to their block

- For transfer transactions: miner will pick transactions with highest gas

- For smart contract transactions: miner will pick w/ highest gas, gas will be spent on contract execution + miner fee

- Vulnerability: miners will pick contract executions only as there may be higher gas due to execution. this may cause transfers with high fee.

## Todo

- Mempool or Transaction Pool

- Update function to calculate balance to work for miner
- Update coinbase transaction to include fixed block reward + transaction blocks fees
- Make a coinbase creation function in Miner module. include reward + fees

- beautiful Show instance for Blockchain
