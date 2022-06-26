data AuthorName = AuthorName {
                  firstName :: String
                , lastName :: String}
                | AuthorNameWithMiddle {
                  firstName :: String
                , middleName :: String
                , lastName :: String} deriving Show

data Book = Book {
            author :: AuthorName
          , isbn :: String
          , title :: String
          , year :: Int
          , price :: Double} deriving Show

