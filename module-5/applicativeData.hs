data User = User
  { name :: String,
    gamerId :: Int,
    score :: Int
  }
  deriving (Show)

serverUsername :: Maybe String
serverUsername = Just "Nick"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

maybeUser = User <$> serverUsername <*> serverGamerId <*> serverScore

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter nickname, user ID, score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

testNames :: [String]
testNames = ["John Smith", "Robert'); DROP TABLE Students;--", "Kristina NULL", "Randel Monro"]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 10000, -99999]

testData :: [User]
testData = User <$> testNames <*> testIds <*> testScores