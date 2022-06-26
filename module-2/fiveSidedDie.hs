data FiveSidedDie = S1 | S2 | S3 | S4 | S5

class SuperDie a => Die a where
    isHeck :: a -> Bool

class SuperDie a where
    isFalse :: Bool

instance SuperDie FiveSidedDie a where
    isFalse = False

instance Die FiveSidedDie a where
    isHeck = \a -> True
