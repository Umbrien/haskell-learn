maxPair :: (Ord a) => (a, a) -> a
maxPair (a, b) = max a b

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM a = a >>= (return . maxPair)

-- maxPairM = fmap maxPair

pair :: (Int, Int)
pair = (1, 2)

maybePair :: Maybe (Int, Int)
maybePair = Just pair

maxMaybe :: Maybe Int
maxMaybe = maxPairM maybePair

ioPair :: IO (Int, Int)
ioPair = return pair

maxIO :: IO Int
maxIO = maxPairM ioPair

listPair :: [(Int, Int)]
listPair = [pair]

maxList :: [Int]
maxList = maxPairM listPair
