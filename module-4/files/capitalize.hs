import Data.Text qualified as T
import Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  TIO.writeFile fileName $ T.toUpper input
