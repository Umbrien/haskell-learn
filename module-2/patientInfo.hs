ageHeight :: Int -> Int -> String
ageHeight age height = "(Age: " ++ show age ++
                     "; height: " ++ show height ++ "sm)"

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ (ageHeight age height)
  where name = lname ++ ", " ++ fname

type FirstName = String
type LastName = String
type PatientName = (FirstName, LastName)

patientInfo3 :: PatientName -> Int -> Int -> String
patientInfo3 name age height = fname ++ " " ++ (ageHeight age height)
  where fname = snd name ++ ", " ++ fst name
