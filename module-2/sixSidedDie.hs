data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
  deriving (Eq, Ord, Enum)

--instance Eq SixSidedDie where 
--  (==) S1 S1 = True
--  (==) S2 S2 = True
--  (==) S3 S3 = True
--  (==) S4 S4 = True
--  (==) S5 S5 = True
--  (==) S6 S6 = True
--  (==) _ _ = False

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"
