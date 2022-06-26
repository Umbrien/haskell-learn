doubleDouble x = d * 2
  where d = x * 2

ldoubleDouble x = (\d -> d * 2) x * 2

idoubleDouble x = let d = x * 2
                  in
                    d * 2


