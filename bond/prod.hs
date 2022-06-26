prod x l = foldr (*) 1 firsts
  where firsts = take x l

fact 0 = 1
fact n = n * fact (n - 1)

