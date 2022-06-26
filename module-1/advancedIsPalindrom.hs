removeSpaces [] = []
removeSpaces (x:xs) = if not(x == ' ')
                      then x : removeSpaces xs
                      else removeSpaces xs

altRemoveSpaces str = filter (\x -> not(x == ' ')) str

charToLowerCase x = case x of
                  'A' -> 'a'
                  'B' -> 'b'
                  'C' -> 'c'
                  'D' -> 'd'
                  'E' -> 'e'
                  'F' -> 'f'
                  'G' -> 'g'
                  'H' -> 'h'
                  'I' -> 'i'
                  'J' -> 'j'
                  'K' -> 'k'
                  'L' -> 'l'
                  'M' -> 'm'
                  'N' -> 'n'
                  'O' -> 'o'
                  'P' -> 'p'
                  'Q' -> 'q'
                  'R' -> 'r'
                  'S' -> 's'
                  'T' -> 't'
                  'U' -> 'u'
                  'V' -> 'v'
                  'W' -> 'w'
                  'X' -> 'x'
                  'Y' -> 'y'
                  'Z' -> 'z'
                  _ -> x

--TODO import strToLowerCase, charToLowerCase in other files
strToLowerCase str = map charToLowerCase str

advIsPalindrom str = lowerStr == reverse lowerStr
  where lowerStr = strToLowerCase (altRemoveSpaces str)

--TODO google toString func

harmonic 0 = 0
harmonic 1 = 1
harmonic n = (harmonic (n-1)) +  (1/n)
