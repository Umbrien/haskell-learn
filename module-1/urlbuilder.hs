getRequestUrl host apiKey resource id =
  "https://" ++ host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

getHostRequestBuilder host =
  (\apiKey resource id -> getRequestUrl host apiKey resource id)

getApiBuilder hostBuilder key =
  (\resource id -> hostBuilder key resource id)

getResourceBuilder hostBuilder key resource =
  (\id -> hostBuilder key resource id)

partial = getRequestUrl "example.com" "1337hAsk3ll" "book"

flipBinaryArgs func = (\y x -> func x y)
xyTest x y = [x, y]

binaryPartialApplication binFunc arg =
  (\missedArg -> binFunc arg missedArg)
