robot (name, attack, hp) = \message ->
                              message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

getN n = \aRobot -> aRobot n
getName = getN name
getAttack = getN attack
getHP = getN hp
getLambdaName = getN (\(n, _, _) -> n)

setName aRobot newName = aRobot (\(_,a,h) -> 
                                  robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n,_,h) ->
                                      robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n,a,_) ->
                              robot (n,a,newHP))

printRobot aRobot = aRobot (\(n, a, h) ->
                              n ++
                              " attack:" ++ (show a) ++
                              " HP:" ++ (show h))
damage aRobot attackDamage = aRobot (\(n,a,h) ->
                                robot (n,a,h - attackDamage))
      
fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0

robotsList = [
  robot ("a", 30, 24),
  robot ("b", 49, 91),
  robot ("c", 92, 12) ]

fourthRobot = robot ("fourth", 50, 100)

fourthFight = fight fourthRobot

fightedList = map fourthFight robotsList

fightedHP = map getHP fightedList

