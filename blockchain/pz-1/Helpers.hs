module Helpers where

import Hash (Hash, hash)
import DataStructures (Block(..), Blockchain, difficulty)

isHashDifficultyValid :: Hash -> Bool
isHashDifficultyValid h = take difficulty h == take difficulty (repeat '0')

blockHash :: Block -> Hash
blockHash (Block index transactions nonce previousHash timeStamp) = hash nonceAndBlockHash
    where
        blockContentsHash = hash $ show index ++ show transactions ++ show previousHash ++ show timeStamp
        nonceAndBlockHash = hash $ show nonce ++ blockContentsHash

-- todo check if transaction is valid:
-- - sender balance is enough for gas + (if Transaction is Transfer, then + amount)
-- - if multiple transactions from one sender in the same block, make sure it will work as should: user cannot fool system and use his balance twice
-- - last miner Transaction is Transfer with amount equal to all fees
isBlockValid :: ([Block], Block) -> Bool
isBlockValid ([], _) = True -- or not just True if other uslovia below?
isBlockValid (blockchain, block) = and [isNonceUnique, isNonceValid] -- todo more uslovia??
    where
        existingProofs = map nonce blockchain
        isNonceUnique = all (\a -> not $ a == nonce block) existingProofs
        isNonceValid = isHashDifficultyValid $ blockHash block
        -- isMinerTransactionValid = and [ ]

addBlock :: Blockchain -> [Block] -> Blockchain
addBlock blockchain blocks = blockchain ++ [approvedBlock]
    where
        validBlocks = filter (\bs -> isBlockValid (blockchain, bs)) blocks
        greatestProof = foldl (\acc curr -> max acc (nonce curr)) 0 blocks
        approvedBlock = head $ filter (\b -> nonce b == greatestProof) validBlocks -- will cause exception if filter resuts in []