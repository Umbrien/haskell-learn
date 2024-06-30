import System.Directory (removeFile)
import System.Environment (getArgs)

genFileVariants :: String -> [String]
genFileVariants s = [s, s ++ ".o", s ++ ".hi"]

main :: IO ()
main = do
  args <- getArgs
  let filesToRemove = concatMap genFileVariants args
  mapM_ removeFile filesToRemove
