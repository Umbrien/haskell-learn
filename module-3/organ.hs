import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen
  deriving (Show, Eq, Ord)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

seventh :: Maybe Organ
seventh = Map.lookup 7 organCatalog

inventoryPairs :: [(Organ, Int)]
inventoryPairs = zip organs ids

organInventory :: Map.Map Organ Int
organInventory = Map.fromList inventoryPairs

brainth :: Maybe Int
brainth = Map.lookup Brain organInventory

