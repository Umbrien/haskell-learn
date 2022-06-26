nyOffice name = nameText ++ " New York"
  where nameText = (fst name) ++ ":: " ++ (snd name)

vaOffice name = nameText ++ " Washington state"
  where nameText = "Great" ++ (fst name) ++ " " ++ (snd name) ++ ","

getLocationFunc location =
  case location of
    "ny" -> nyOffice
    "va" -> vaOffice
    _ -> (\name ->
      (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunc name
  where locationFunc = getLocationFunc location

