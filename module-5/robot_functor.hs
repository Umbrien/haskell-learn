import Data.Map qualified as Map

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left hand for hitting in the face!",
      cost = 1000,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "right hand for good gestures",
      cost = 1025,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "this head looks crazy",
      cost = 5092.25,
      count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>",
      partName,
      "</h2>",
      "<p><h3>desc</h3>",
      partDesc,
      "</p><p><h3>cost</h3>",
      partCost,
      "</p><p><h3>count</h3>",
      partCount,
      "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show $ cost part
    partCount = show $ count part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1 ..]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

insertSnippet :: Maybe Html -> IO ()
insertSnippet = undefined

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

-- allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- same as
-- allPartsHtml = map renderHtml allParts
-- in List context, fmap is same as map

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

printInt :: Maybe String -> IO ()
printInt Nothing = putStrLn "No, nothing"
printInt (Just val) = putStrLn val

processInt :: Int -> String
processInt i = s ++ "!"
  where
    s = show $ i ^ 2

maybeInt :: Maybe Int
maybeInt = Just 3

processedInt :: Maybe String
processedInt = processInt <$> maybeInt

printProcessedInt :: IO ()
printProcessedInt = do
  printInt processedInt

data Box a = Box a deriving (Show)

instance Functor Box where
  fmap func (Box a) = Box (func a)

morePresents :: Box a -> Box [a]
morePresents a = (\a -> [a, a, a]) <$> a

myBox :: Box Int
myBox = Box 1

wrapped = Box <$> myBox

unwrap :: Box a -> a
unwrap (Box a) = a

-- main = do
--   inputStream <- getLine
--   let requests = map read $ lines inputStream
--   let costs = map (\r -> cost <$> Map.lookup r partsDB) requests
--   print costs

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Component not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
  putStrLn "Component id:"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost $ cost <$> part
