n = Name "George" "Marlow"
c = AuthorCreator (Author n)
--b = Book c "isbn" "title" 2022 109.2
b = Book { author = c, isbn = "isbn", bookYear = 2022,
         bookTitle = "title", bookPrice = 203.3 }
-----

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithInit FirstName Char
      deriving Show

data Author = Author Name deriving Show
            
data Artist = Person Name | Band [Name] deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist
      deriving Show

hpLoveCraft :: Creator
hpLoveCraft = AuthorCreator 
                (Author
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book {
            author :: Creator
          , isbn :: String
          , bookTitle :: String
          , bookYear :: Int
          , bookPrice :: Double} deriving Show
bprice :: Book -> Double
bprice (Book _ _ _ _ p) = p

data VinylRecord = VinylRecord {
                   artist :: Creator
                 , recordTitle :: String
                 , recordYear :: Int
                 , recordPrice :: Double}

data CollectibleToy = CollectibleToy {
                      name :: String
                    , description :: String
                    , toyPrice :: Double}

-- broshura
data Pamphlet = Pamphlet {
  pamphletTitle :: String,
  pampletDescription :: String,
  contact :: String
}

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

price :: StoreItem -> Double
--price (BookItem book) = bookPrice book
price (BookItem (Book _ _ _ _ p)) = p
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy (ToyItem toy) = show (name toy)
madeBy _ = "unknown"

