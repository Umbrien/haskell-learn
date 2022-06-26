srez list from to =
    take (to - from) (drop from list)

inFirstHalf element list = element `elem` firstHalf
  where firstHalf = take ((length list) `div` 2) list

myTail (_:xs) = xs
myTail [] = []

myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

mg a 0 = a
mg a b = mg b (a `mod` b)

