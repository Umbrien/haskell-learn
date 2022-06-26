data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

data BreakfastSpecial = Children BreakfastMain BreakfastSide
                      | Standard BreakfastMain BreakfastMeat BreakfastSide
                      | Drovosek BreakfastMain BreakfastMain BreakfastMeat BreakfastMeat BreakfastSide BreakfastSide BreakfastSide
  deriving Show

