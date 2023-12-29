module Helpers where

import Hash (Hash, hash)
import DataStructures (Address, Transaction(..), TransactionType(..), Block(..), Blockchain, difficulty, coinbaseReward)

isHashDifficultyValid :: Hash -> Bool
isHashDifficultyValid h = take difficulty h == take difficulty (repeat '0')

blockHash :: Block -> Hash
blockHash = hash . show

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

isCoinbaseRewardFair :: TransactionType -> Bool
isCoinbaseRewardFair (Coinbase reward) = reward == coinbaseReward
isCoinbaseRewardFair _ = False

-- todo check more conditions:
-- - sender balance is enough for gas + (if Transaction is Transfer, then + amount)
-- - if multiple transactions from one sender in the same block, make sure it will work as should: user cannot fool system and use his balance twice (double spending)
-- - validate hashes are correct through isNonceValid
isBlockValid :: (Blockchain, Block) -> Bool
isBlockValid ([], _) = True -- genesis block is valid
isBlockValid (blockchain, block) = and [isNonceUnique, isNonceValid, isCoinbaseValid]
  where
    existingProofs = map nonce blockchain
    isNonceUnique = all (\a -> not $ a == nonce block) existingProofs
    isNonceValid = isHashDifficultyValid $ blockHash block
    --
    txBodies = map (\t -> body t) $ transactions block
    isCoinbaseValid = and [
        isCoinbasePlacedValid txBodies
        , isCoinbaseRewardFair $ head txBodies]

addBlock :: Blockchain -> [Block] -> Blockchain
addBlock blockchain blocks = case approvedBlock of
    Just block -> blockchain ++ [block]
    Nothing -> blockchain
  where
    validBlocks = filter (\bs -> isBlockValid (blockchain, bs)) blocks
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
