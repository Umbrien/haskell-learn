-- unused, required for compiling
main :: IO ()
main = return ()

-- compile with -Wall option: ghc -Wall headaches.hs
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x : xs) = x : myTakePM (n - 1) xs

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x : _) = x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

a :: Maybe Integer
a = (+ 2) <$> maybeHead [1]

a' :: Maybe Integer
a' = (+ 2) <$> maybeHead []

b :: Maybe [Integer]
b = (:) <$> maybeHead [1, 2, 3] <*> pure []

b' :: Maybe [a]
b' = (:) <$> maybeHead [] <*> pure []

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) =
  (:)
    <$> maybeHead xs
    <*> myTakeSafer (n - 1) (Just (tail xs))

eitherHead :: [a] -> Either String a
eitherHead [] = Left "Empty list"
eitherHead (x : _) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

sumEitherTail :: Either String Int
sumEitherTail = (+) <$> eitherHead intExample <*> eitherHead (tail intExample)

sumEitherTail' :: Either String Int
sumEitherTail' = (+) <$> eitherHead intExampleEmpty <*> eitherHead (tail intExample)

-- will cause runtime error
sumEitherTail'' :: Either String Int
sumEitherTail'' = (+) <$> eitherHead intExample <*> eitherHead (tail intExampleEmpty)