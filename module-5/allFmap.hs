allFmap :: (Applicative f) => (a -> b) -> f a -> f b
allFmap f a = pure f <*> a

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 6

yesterdayBought :: [Int]
yesterdayBought = [6, 12]

nightDrink :: Int
nightDrink = 4

todayFriends :: [Int]
todayFriends = [2, 3]

bottlesPerPerson :: [Int]
bottlesPerPerson = [3, 4]

bottlesLeft :: [Int]
bottlesLeft = (-) <$> yesterdayBought <*> pure nightDrink

totalPeople :: [Int]
totalPeople = (+ 2) <$> todayFriends

bottlesRequired :: [Int]
bottlesRequired = (*) <$> totalPeople <*> bottlesPerPerson

bottlesToBuy :: [Int]
bottlesToBuy = (-) <$> bottlesRequired <*> bottlesLeft
