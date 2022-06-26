add3toall [] = []
add3toall (x:xs) = (3 + x) : add3toall xs

myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

remove f [] = []
remove f (x:xs) = if not (f x)
                  then x : remove f xs
                  else remove f xs

myProduct xs = foldl (*) 1 xs

myElem val list = length(filter (\x -> x == val) list) > 0

removeSpaces s = map (\x -> if x == ' '
                            then 's'
                            else x) s
