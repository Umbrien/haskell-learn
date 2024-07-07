{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO

type Author = T.Text

type Title = T.Text

data Book = Book {author :: Author, title :: Title} deriving (Show)

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

book1ru :: Book
book1ru = Book {title = "Заговор против человека", author = "Лиготти, Томас"}

book1en :: Book
book1en = Book {title = "The Conspiracy Against the Humanity", author = "Ligotti, Thomas"}

book2ru :: Book
book2ru = Book {title = "Трактат о разложении", author = "Чоран, Эмиль"}

book2en :: Book
book2en = Book {title = "A Short History of Decay", author = "Cioran, Emil"}

book3ru :: Book
book3ru = Book {title = "Слёзы эроса", author = "Батай, Жорж"}

book3en :: Book
book3en = Book {title = "The Tears of Eros", author = "Bataille, Georges"}

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n",
      "<head><title>Books</title>",
      "<meta charset='utf-8' />",
      "</head>\n",
      "<body>\n",
      booksHtml,
      "\n</body>\n</html>"
    ]
  where
    booksHtml = (mconcat . map bookToHtml) books

myBooksEn :: [Book]
myBooksEn = [book1en, book2en, book3en]

myBooksRu :: [Book]
myBooksRu = [book1ru, book2ru, book3ru]

leaderLength :: Int
leaderLength = 24

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt $ B.take 5 leader

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream =
  if marcStream == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

main1 :: IO ()
main1 = do
  TIO.writeFile "books-en.html" (booksToHtml myBooksEn)
  TIO.writeFile "books-ru.html" (booksToHtml myBooksRu)
  marcData <- B.readFile "sample.mrc"
  let marcRecords = allRecords marcData
  print $ length marcRecords
  print $ getBaseAddress $ marcRecords !! 409

-- Record Catalog
type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt $ B.take 5 remainder
  where
    remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if directory == B.empty
    then []
    else nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) = B.splitAt dirEntryLength directory

type Tag = T.Text

data FieldMetadata = FieldMetadata
  { tag :: Tag,
    fieldLength :: Int,
    fieldStart :: Int
  }
  deriving (Show)

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

fieldDelimeter :: Char
fieldDelimeter = toEnum 31

titleTag :: Tag
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: Tag
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: Tag -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if null results
    then Nothing
    else Just $ head results
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
  if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetadata
    subFields = T.split (== fieldDelimeter) rawField
    results = filter ((== subfield) . T.head) subFields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
  map
    ( \(title, author) ->
        Book
          { title = fromJust title,
            author = fromJust author
          }
    )
    justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 50 marcData
  TIO.writeFile "books.html" processed
