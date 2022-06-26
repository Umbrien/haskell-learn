data Sex = Male | Female deriving (Show)

data RhType = Pos | Neg
instance Show RhType where
    show Pos = "+"
    show Neg = "-"

data ABOType = A | B | AB | O deriving (Show)

data BloodType = BloodType ABOType RhType
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = show abo ++ show rh

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType
                       }

-- Donor rules
-- 1 - 1, 2, 3, 4
-- 2 - 2, 4
-- 3 - 3, 4
-- 4 - 4
canDonateto :: BloodType -> BloodType -> Bool
canDonateto (BloodType O _) _ = True -- universal donor
canDonateto _ (BloodType AB _) = True -- universal receiver
canDonateto (BloodType A _) (BloodType A _) = True
canDonateto (BloodType B _) (BloodType B _) = True
canDonateto _ _ = False -- all other variants

canBeDonorFor :: Patient -> Patient -> Bool
canBeDonorFor p1 p2 = canDonateto (bloodType p1) (bloodType p2)

patientSummary :: Patient -> String
patientSummary patient = "********\n" ++
                         "Patient name: " ++ showName (name patient) ++ "\n" ++
                         "Age : " ++ show (age patient) ++ "\n" ++
                         "Height: " ++ show (height patient) ++ "sm\n" ++
                         "Weight: " ++ show (weight patient) ++ "kg\n" ++
                         "Blood type: " ++ showBloodType (bloodType patient) ++
                         "\n********\n"

----------------------------

nikita :: Patient
nikita = Patient { name = NameWithMiddle "Nikita" "Solomatin" "Yunakov"
                 , sex = Male
                 , age = 24
                 , height = 173
                 , weight = 92
                 , bloodType = BloodType O Neg }

lera :: Patient
lera = Patient { name = Name "Lera" "Shibika"
               , sex = Female
               , age = 18
               , height = 159
               , weight = 62
               , bloodType = BloodType A Pos }

main :: IO()
main = do
    putStr (patientSummary nikita)
    putStr (patientSummary lera)

