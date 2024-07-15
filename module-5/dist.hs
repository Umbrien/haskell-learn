import Data.Map qualified as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkhem", (42.6054, -70.7829)),
      ("Insmut", (42.8250, -70.8150)),
      ("Karkoza", (29.9714, -90.7694)),
      ("New York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a =
      (sin (dlat / 2)) ^ 2
        + cos rlat1
          * cos rlat2
          * (sin (dlong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 6378.1

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error: city not found"
printDistance (Just dist) = putStrLn $ show dist ++ " km"

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just $ a + b
addMaybe _ _ = Nothing

maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1

maybe6 :: Maybe Integer
maybe6 = maybeInc <*> Just 5

maybeNothing :: Maybe Integer
maybeNothing = maybeInc <*> Nothing

catsDogs :: Maybe String
catsDogs = (++) <$> Just "cats" <*> Just " and dogs"

val1 = Just 10

val2 = Just 5

starred = (*) <$> val1 <*> val2

divved = div <$> val1 <*> val2

apply2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
apply2 f a b = f <$> a <*> b

modded = apply2 mod val1 val2

main :: IO ()
main = do
  putStrLn "First city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Second city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ioA ioB = do
  a <- ioA
  b <- ioB
  let dist = haversine a b
  return dist

haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' a b = haversine <$> a <*> b