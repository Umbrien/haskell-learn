mysDrop 0 xs = xs
mysDrop num xs = mysDrop (num - 1) (tail xs)

myDrop num xs = if num == 0
                then xs
                else myDrop (num - 1) (tail xs)

myCycle (x:xs) = x : myCycle(xs ++ [x])

-- usage: r n
reclimit l 0 = l
reclimit l n = reclimit (n:l) (n-1)
r = reclimit []

newr 0 = []
newr n = n : newr (n - 1)

myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]
