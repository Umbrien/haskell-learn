
counter x =  (\x ->
              (\x -> x + 1) x + 1) x

ifEven func x = if even x
                then func x
                else x

ifEvenDouble = ifEven (\x -> x*2)

x3 x = (\x -> x^3) x

