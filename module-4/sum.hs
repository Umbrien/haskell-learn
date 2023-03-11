import System.Environment (getArgs)
import Control.Monad (replicateM)

{-
- getArgs: get command line arguments
-
- replicateM n f - repeat IO function n times, returns IO []
-}

main1 :: IO ()
main1 = do
  args <- getArgs
  mapM_ putStrLn args

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n f = mapM (\_ -> f) [1 .. n]

main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                      then read $ head args
                      else 0 :: Int
  numbers <- myReplicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  putStrLn "- - -"
  print $ sum ints


