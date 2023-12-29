module Helpers where

import Hash (Hash, hash)
import DataStructures (Address, Transaction(..), TransactionType(..), Block(..), Blockchain, difficulty, blockReward)

hashDifficultyValid :: Hash -> Bool
hashDifficultyValid h = take difficulty h == take difficulty (repeat '0')

blockHash :: Block -> Hash
blockHash = hash . show

coinbaseReward :: [Transaction] -> Float
coinbaseReward transactions = blockReward + transactionsGasSum transactions
  where
    transactionsGasSum :: [Transaction] -> Float
    transactionsGasSum = sum . map (\(Transaction _ gas _) -> gas)

-- Check if there's only one Coinbase and it's the first transaction
isCoinbasePlacedValid :: [TransactionType] -> Bool
isCoinbasePlacedValid [] = False
isCoinbasePlacedValid (Coinbase _ : xs) = noCoinbase xs
  where
    noCoinbase :: [TransactionType] -> Bool
    noCoinbase [] = True
    noCoinbase (Coinbase _ : _) = False
    noCoinbase (_ : xxs) = noCoinbase xxs
isCoinbasePlacedValid _ = False

isCoinbaseRewardFair :: [Transaction] -> Bool
isCoinbaseRewardFair (Transaction _ _ (Coinbase reward):restTransactions) = reward == coinbaseReward restTransactions
isCoinbaseRewardFair _ = False

-- isNewBlockValid is function for validating new blocks. As system assumes that genesis block is valid, it should not be used with genesis block.
-- todo check more conditions:
-- - sender balance is enough for gas + (if Transaction is Transfer, then + amount)
-- - if multiple transactions from one sender in the same block, make sure it will work as should: user cannot fool system and use his balance twice (double spending)
isNewBlockValid :: Block -> Block -> Bool
isNewBlockValid previousBlock block = and [isNonceValid, isCoinbaseValid]
  where
    isNonceValid = and [isHashDifficultyValid, isHashValid ]
    isHashDifficultyValid = hashDifficultyValid $ blockHash block
    isHashValid = blockHash previousBlock == previousHash block
    --
    txBodies = map (\t -> body t) $ transactions block
    isCoinbaseValid = and [
        isCoinbasePlacedValid txBodies
        , isCoinbaseRewardFair $ transactions block]

addBlock :: Blockchain -> [Block] -> Blockchain
addBlock blockchain blocks = case approvedBlock of
    Just block -> blockchain ++ [block]
    Nothing -> blockchain
  where
    validBlocks = filter (\bs -> isNewBlockValid (last blockchain) bs) blocks
    greatestProof = foldl (\acc curr -> max acc (nonce curr)) 0 blocks
    approvedBlock :: Maybe Block
    approvedBlock = case filter (\b -> nonce b == greatestProof) validBlocks of
        (x : _) -> Just x
        [] -> Nothing

balance :: Blockchain -> Address -> Float
balance blockchain address = toAddressTransfersSum + transactionEffects
  where
    blockchainTransactions = foldl (\acc b -> acc ++ transactions b) [] blockchain
    toAddressTransfersSum = foldl (\acc curr -> acc + amount curr) 0 toAddressTransfers
      where
        toAddressTransfers = filter (\t -> isTransferToAddress t) transationBodies
        transationBodies = map (\t -> body t) blockchainTransactions
        isTransferToAddress :: TransactionType -> Bool
        isTransferToAddress (Transfer to _) = to == address
        isTransferToAddress _ = False
    transactionEffects = foldl (\acc tx -> acc + transactionEffect tx) 0 addressAuthoredTransactions
      where
        addressAuthoredTransactions = filter (\t -> author t == address) blockchainTransactions
        transactionEffect :: Transaction -> Float
        transactionEffect (Transaction _ _ (Coinbase reward)) = reward
        transactionEffect (Transaction _ gas (Transfer _ amount)) = -amount - gas
        transactionEffect (Transaction _ gas (SmartContractCall _ _)) = -gas
