module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple
  ( Request,
    defaultRequest,
    getResponseBody,
    getResponseStatus,
    getResponseStatusCode,
    httpLBS,
    setRequestHeader,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
    setRequestPort,
    setRequestSecure,
  )
import Network.HTTP.Types.Status (statusCode, statusMessage)

-- import Lib

myToken :: BC.ByteString
myToken = "<api-token>"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest ::
  BC.ByteString ->
  BC.ByteString ->
  BC.ByteString ->
  BC.ByteString ->
  Request
buildRequest token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestSecure True $
            setRequestPort
              443
              defaultRequest

buildRequestNOSSL ::
  BC.ByteString ->
  BC.ByteString ->
  BC.ByteString ->
  BC.ByteString ->
  Request
buildRequestNOSSL token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestPort
            443
            defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

main :: IO ()
main = do
  response <- httpLBS request
  let responseStatusCode = getResponseStatusCode response
  if responseStatusCode == 200
    then do
      putStrLn "request results saved to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else do
      putStrLn "query failed due to error"
      let status = getResponseStatus response
      putStrLn $ "status " ++ show (statusCode status)
      print $ statusMessage status