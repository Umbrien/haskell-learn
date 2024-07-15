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