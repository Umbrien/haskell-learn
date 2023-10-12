## Assumptions

- As of development time, there is single-node architecture. I hope it will change to decentralised

- Proof of Work realisation: biggest proposed number wins

- At the end of the block, miner includes transaction to himself with author 0x000 with a sum of gas from prev transactions or less, then it will be burnt

- Genesis block generates some balance out of nowhere for demo purposes
  as well as does not include last block for miner as there are no miners???

## How it will work for user

- There is transaction pool from which miners will pick transactions they want to include to their block

- For transfer transactions: miner will pick transactions with highest gas

- For smart contract transactions: miner will pick w/ highest gas, gas will be spent on contract execution + miner fee

- Vulnerability: miners will pick contract executions only as there may be higher gas due to execution? this may cause transfers with high fee?
