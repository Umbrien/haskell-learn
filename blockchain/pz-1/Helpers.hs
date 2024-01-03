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

-- Function for validating new blocks.
-- As system assumes that genesis block is valid, it should not be used with genesis block.
isNewBlockValid :: Blockchain -> Block -> Bool
isNewBlockValid blockchain block = and [isNonceValid, isCoinbaseValid]
  where
    isNonceValid = and [isHashDifficultyValid, isHashValid ]
    isHashDifficultyValid = hashDifficultyValid $ blockHash block
    isHashValid = blockHash (last blockchain) == previousHash block
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
    validBlocks = filter (\bs -> isNewBlockValid blockchain bs) blocks
    greatestProof = foldl (\acc curr -> max acc (nonce curr)) 0 blocks
    approvedBlock :: Maybe Block
    approvedBlock = case filter (\b -> nonce b == greatestProof) validBlocks of
        (x : _) -> Just x
        [] -> Nothing

balance :: Blockchain -> Address -> Float
balance blockchain address = txsEffect address blockchainTransactions
  where
    blockchainTransactions = foldl (\acc b -> acc ++ transactions b) [] blockchain

txEffect :: Address -> Transaction -> Float
txEffect address (Transaction author gas (Transfer to amount))
 | author == address && to == address = -gas
 | author == address = -amount - gas
 | to == address = amount
 | otherwise = 0
txEffect address (Transaction author gas (SmartContractCall _ _))
 | author == address = -gas
 | otherwise = 0
txEffect address (Transaction author _ (Coinbase reward))
 | author == address = reward
 | otherwise = 0

txsEffect :: Address -> [Transaction] -> Float
txsEffect address = sum . map (\t -> txEffect address t)