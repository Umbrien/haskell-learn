fib :: Int -> Int
fib 0 = 1
fib 1 = 2
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci at position:"
    num <- getLine
    let fibNum = fib $ read num
    putStrLn $ show fibNum


