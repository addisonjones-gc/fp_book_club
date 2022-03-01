robot (name,attack,hp) = \message -> message (name,attack,hp)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

describeRobot aRobot = aRobot (\(n,a,h) -> n ++
                                           " attack:" ++ (show a) ++
                                           " hp:" ++ (show h))

-- printHPs :: [robot] -> IO()
-- printHPs robotList = print(map (\r -> show (getName r) ++ " HP: " ++ show (getHP r)) robotList)
printHPs robotList = print(map (\r -> getName r ++ " HP: " ++ show (getHP r)) robotList)
-- printHPs robotList = print(map (getHP) robotList)

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))

fight aRobot defender = let attack = if getHP aRobot > 10
                                     then getAttack aRobot
                                     else 0
                         in damage defender attack

killerRobot = robot ("Kill3r", 25, 200)
gentleRobot = robot ("Mr. Friendly", 10, 300)

main = do 
    let myKiller = killerRobot
    let gentleGiant = gentleRobot
    let myKiller = setName killerRobot "NewGuy"
    let afterHit = damage myKiller 90
    -- print (describeRobot afterHit)

    let gentleGiantRound1 = fight killerRobot gentleGiant
    let killerRobotRound1 = fight gentleGiant killerRobot
    let gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
    let killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
    let gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
    let killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

    -- print (describeRobot gentleGiantRound3)
    -- print (describeRobot killerRobotRound3)
    
    printHPs [gentleGiantRound3, killerRobotRound3]   