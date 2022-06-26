data Sex = Male | Female

a :: Sex
a = Male

b :: Sex
b = Female

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BleedType ABOType RhType

t1 :: BloodType
t1 = BleedType A Neg

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BleedType abo rh) = showABO abo ++ showRh rh
