import Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let fromFileName = head args
  let toFileName = args !! 1
  input <- TIO.readFile fromFileName
  TIO.writeFile toFileName input
