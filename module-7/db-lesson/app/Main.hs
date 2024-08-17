{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Data.Time (Day, getCurrentTime, utctDay)
import Database.SQLite.Simple (Connection, Only (Only), Query, close, execute, open, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), RowParser, field)

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesBorrowed :: Int
  }

instance FromRow Tool where
  fromRow :: RowParser Tool
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

data User = User
  { userId :: Int,
    userName :: String
  }

instance FromRow User where
  fromRow :: RowParser User
  fromRow = User <$> field <*> field

instance Show User where
  show user =
    mconcat
      [ show $ userId user,
        ".) ",
        userName user
      ]

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool,
        ".) ",
        name tool,
        "\n description: ",
        description tool,
        "\n last time returned: ",
        show $ lastReturned tool,
        "\n times borrowed: ",
        show $ timesBorrowed tool,
        "\n"
      ]

db :: String
db = "tools.db"

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

addUser :: String -> IO ()
addUser _userName = withConn db $ \conn -> do
  execute conn "INSERT INTO users (username) VALUES (?)" (Only _userName)
  putStrLn $ mconcat ["user added: ", _userName]

checkout :: Int -> Int -> IO ()
checkout _userId _toolId = withConn db $ \conn ->
  execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)" (_userId, _toolId)

printUsers :: IO ()
printUsers = withConn db $ \conn -> do
  resp <- query_ conn "SELECT * FROM users;" :: IO [User]
  mapM_ print resp

addTool :: String -> String -> IO ()
addTool toolName toolDescription = withConn db $ \conn -> do
  today <- utctDay <$> getCurrentTime
  let _timesBorrowed :: Int
      _timesBorrowed = 0
  execute
    conn
    "INSERT INTO tools (name, description, lastReturned, timesBorrowed) VALUES (?,?,?,?)"
    (toolName, toolDescription, today, _timesBorrowed)
  putStrLn $ toolName ++ " tool added"

printToolQuery :: Query -> IO ()
printToolQuery q = withConn db $ \conn -> do
  resp <- query_ conn q :: IO [Tool]
  mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable =
  printToolQuery $
    mconcat
      [ "select * from tools ",
        "where id not in ",
        "(select tool_id from checkedout);"
      ]

printCheckedout :: IO ()
printCheckedout =
  printToolQuery $
    mconcat
      [ "select * from tools ",
        "where id in ",
        "(select tool_id from checkedout);"
      ]

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : _) = Just x

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn _toolId = do
  resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only _toolId) :: IO [Tool]
  return $ firstOrNothing resp

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool
    { lastReturned = date,
      timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "tool id not found"
updateOrWarn (Just tool) = withConn db $ \conn -> do
  let q =
        mconcat
          [ "UPDATE TOOLS SET ",
            "lastReturned = ?,",
            " timesBorrowed = ? ",
            "WHERE ID = ?;"
          ]
  execute
    conn
    q
    ( lastReturned tool,
      timesBorrowed tool,
      toolId tool
    )
  putStrLn "tool data updated"

updateToolTable :: Int -> IO ()
updateToolTable _toolId = withConn db $ \conn -> do
  tool <- selectTool conn _toolId
  currentDay <- utctDay <$> getCurrentTime
  let updatedTool = updateTool <$> tool <*> pure currentDay
  updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin _toolId = withConn db $ \conn ->
  execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only _toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate _toolId = do
  checkin _toolId
  updateToolTable _toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "New user name:"
  _userName <- getLine
  addUser _userName

promptAndAddTool :: IO ()
promptAndAddTool = do
  putStrLn "Tool name:"
  toolName <- getLine
  putStrLn "Tool description:"
  toolDescription <- getLine
  addTool toolName toolDescription

promptAndCheckout :: IO ()
promptAndCheckout = do
  putStrLn "Enter user id:"
  _userId <- read <$> getLine
  putStrLn "Enter tool id:"
  _toolId <- read <$> getLine
  checkout _userId _toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  putStrLn "Enter tool id:"
  _toolId <- read <$> getLine
  checkinAndUpdate _toolId

performCommand :: String -> IO ()
performCommand "users" = printUsers >> main
performCommand "tools" = printTools >> main
performCommand "adduser" = promptAndAddUser >> main
performCommand "addtool" = promptAndAddTool >> main
performCommand "checkout" = promptAndCheckout >> main
performCommand "checkin" = promptAndCheckin >> main
performCommand "in" = printAvailable >> main
performCommand "out" = printCheckedout >> main
performCommand "quit" = putStrLn "Goodbye"
performCommand _ = putStrLn "Command not found" >> main

main :: IO ()
main = do
  putStrLn "Command:"
  command <- getLine
  performCommand command
