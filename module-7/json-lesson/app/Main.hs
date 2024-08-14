module Main (main) where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object),
    decode,
    eitherDecode,
    encode,
    object,
    (.:),
    (.=),
  )
import Data.ByteString.Lazy as B (readFile)
import Data.ByteString.Lazy.Char8 as BC (ByteString)
import Data.Text as T (Text)
import GHC.Generics (Generic)
import Lib

data Book = Book
  { title :: T.Text,
    author :: T.Text,
    year :: Int
  }
  deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook :: Book
myBook = Book {author = "Will Kurt", title = "GPiH", year = 2019}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Will Kurt\",\"title\":\"GPiH\",\"year\":2019}"

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Will Kurt\",\"title\":\"GPiH\",\"year\":2019}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

bookFromWrongJSON :: Either String Book
bookFromWrongJSON = eitherDecode wrongJSON

data Name = Name
  { firstName :: T.Text,
    lastName :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Name

instance ToJSON Name

will :: Name
will = Name "Will" "Kurt"

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage
  { message :: T.Text,
    errorCode :: Int
  }
  deriving (Show)

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage
      <$> v .: "message"
      <*> v .: "error"
  parseJSON _ = fail "Expected an object for ErrorMessage"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage _message _errorCode) =
    object
      [ "message" .= _message,
        "error" .= _errorCode
      ]

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "We're fine" 0

sampleErrorMessageFine :: BC.ByteString
sampleErrorMessageFine = encode anErrorMessage

data NOAAResult = NOAAResult
  { uid :: T.Text,
    mindate :: T.Text,
    maxdate :: T.Text,
    name :: T.Text,
    datacoverage :: Float,
    resultId :: T.Text
  }
  deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult
      <$> v .: "uid"
      <*> v .: "mindate"
      <*> v .: "maxdate"
      <*> v .: "name"
      <*> v .: "datacoverage"
      <*> v .: "id"
  parseJSON _ = fail "Expected an object for NOAAResult"

instance ToJSON NOAAResult where
  toJSON (NOAAResult u mi ma n d rid) =
    object
      [ "uid" .= u,
        "mindate" .= mi,
        "maxdate" .= ma,
        "name" .= n,
        "datacoverage" .= d,
        "id" .= rid
      ]

data Resultset = Resultset
  { offset :: Int,
    count :: Int,
    limit :: Int
  }
  deriving (Show, Generic)

instance FromJSON Resultset

instance ToJSON Resultset

data Metadata = Metadata
  { resultset :: Resultset
  }
  deriving (Show, Generic)

instance FromJSON Metadata

instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata,
    results :: [NOAAResult]
  }
  deriving (Show, Generic)

instance FromJSON NOAAResponse

instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = putStrLn "error loading data"
printResults (Just _results) = forM_ _results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

data IntList = Cons Int IntList | EmptyList deriving (Show, Generic)

instance ToJSON IntList

instance FromJSON IntList

intListExample :: IntList
intListExample =
  Cons 1 $
    Cons 2 EmptyList