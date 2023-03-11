main :: IO ()
main = do
  lines <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn lines

