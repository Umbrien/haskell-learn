data Events = Events [String]
data Probs = Probs [Double]

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events $ cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs $ cartCombine (*) p1 p2

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mempty = Probs []

--- ---

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = Probs $ map (/ totalProbs) probs
        --normalizedProbs = Probs (map (\x -> x / totalProbs) probs)

instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
      where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
      where newEvents = combineEvents e1 e2
            newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable (Events []) (Probs [])

--- ---

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show (prob * 100), "%\n"]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c] 
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

coin :: PTable
coin = createPTable (Events ["орёл", "решка"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["красный", "синий", "зелёный"]) (Probs [0.1, 0.2, 0.7])


-- try: coin <> spinner
--      mconcat [coin, coin, coin]

