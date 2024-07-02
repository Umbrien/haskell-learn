import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- B.readFile fileName
  Prelude.putStr "Bytes: "
  print $ B.length input
  Prelude.putStr "Characters: "
  print $ (T.length . E.decodeUtf8) input
