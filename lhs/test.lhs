This is Bird-style where I have to leave a blank before the code.

> fact :: Integer -> Integer
> fact 0 = 1
> fact n = n * fact (n - 1)

And I leave a blank line after the code.

Let's try some output

> f = fact 3
#> show f

Will it work?
Okay so it will work in GHCI as expected (except show)
but for compilation or runhaskell we need to specify main
