import Control.Monad (foldM)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Glitch (randomReplaceByte, randomReverseBytes, randomSortSection)
import System.Environment (getArgs)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte,
    randomSortSection,
    randomReplaceByte,
    randomReverseBytes,
    randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  putStrLn "Ready!"
