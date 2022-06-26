type Aname = (String, String)

newtype Name = Name (String, String)
  deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) =
      compare (l1, f1) (l2, f2)

names :: [Name]
names = [ Name ("Ien", "Kertis")
        , Name ("Bob", "Dylan")
        , Name ("Eric", "Stetham")
        ]

anames :: [Aname]
anames = [ ("Ien", "Kertis")
         , ("Bob", "Dylan")
         , ("Eric", "Stetham")
         ]
