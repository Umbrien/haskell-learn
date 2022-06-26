import Data.List

l = [("Ben", "Lee"),
     ("Arthur", "Po"),
     ("Stuart", "Little"),
     ("Alex", "Pit"),
     ("Bob", "Pit")
    ]

-- compareLastNames n1 n2 = if lastName1 > lastName2
--                        then GT
--                        else if lastName1 < lastName2
--                            then LT
--                            else EQ
--
compareLastNames n1 n2 = compare lastName1 lastName2
  where lastName1 = snd n1
        lastName2 = snd n2


