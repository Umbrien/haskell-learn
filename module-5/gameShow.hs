doorPrizeDetermined = 1000

boxPrizeDetermined = 500

totalPrizeDeterministic :: Int
totalPrizeDeterministic = (+) doorPrizeDetermined boxPrizeDetermined

-- [Int] in prizes is context: prize is non-deterministic value

doorPrize :: [Int]
doorPrize = [1000, 2000, 300]

boxPrize :: [Int]
boxPrize = [500, 20000]

-- non-deterministic totalPrize is list of all possible values
totalPrize :: [Int]
-- totalPrize = pure (+) <*> doorPrize <*> boxPrize
totalPrize = (+) <$> doorPrize <*> boxPrize

boxMultiplier :: [Int]
boxMultiplier = [10, 50]

totalPrizeMulti :: [Int]
totalPrizeMulti = (*) <$> doorPrize <*> boxMultiplier

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = (*) <$> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)