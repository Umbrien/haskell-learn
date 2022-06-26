test :: String -> Bool
test num = (mod pnum 13) == 0
  where pnum = read num :: Int

