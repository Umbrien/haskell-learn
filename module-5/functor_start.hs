successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just s) = Just (reverse s)
reverseMaybe Nothing = Nothing

fincMaybe :: Maybe Int -> Maybe Int
fincMaybe = fmap (+ 1)

-- <$> is fmap
ffincMaybe :: Maybe Int -> Maybe Int
ffincMaybe a = (+ 1) <$> a

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

maybeReversedNothing :: Maybe String
maybeReversedNothing = reverse <$> Nothing

maybeReversedJust :: Maybe String
maybeReversedJust = reverse <$> Just "string"
