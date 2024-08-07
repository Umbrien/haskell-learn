import Data.Map qualified as Map

areaByDiameter :: Double -> Double
areaByDiameter diameter = pi * radius ^ 2
  where
    radius = diameter / 2

type Pizza = (Double, Double)

costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaByDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerCm p1
    costP2 = costPerCm p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "Pizza of size "
    ++ show size
    ++ " is cheaper with price "
    ++ show costSqCm
    ++ " per cm2"
  where
    costSqCm = costPerCm (size, cost)

main :: IO ()
main = do
  putStrLn "First pizza size:"
  size1 <- getLine
  putStrLn "First pizza cost:"
  cost1 <- getLine
  putStrLn "Second pizza size:"
  size2 <- getLine
  putStrLn "Second pizza cost:"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn $ describePizza betterPizza

costData :: Map.Map Int Double
costData = Map.fromList [(1, 150), (2, 220)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 30), (2, 50)]

maybeMain :: Maybe String
maybeMain = do
  cost1 <- Map.lookup 1 costData
  size1 <- Map.lookup 1 sizeData
  cost2 <- Map.lookup 2 costData
  size2 <- Map.lookup 2 sizeData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza

-- with return, String becomes Maybe String (type of do notation)

listMain :: [String]
listMain = do
  size1 <- [12.0, 15.0, 20.0]
  cost1 <- [12.0, 15.0, 20.0]
  size2 <- [10, 11, 18]
  cost2 <- [13.0, 14.0, 21.0]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza

monadMain :: (Monad m) => m Double -> m Double -> m Double -> m Double -> m String
monadMain s1 c1 s2 c2 = do
  cost1 <- c1
  size1 <- s1
  cost2 <- c2
  size2 <- s2
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza

mainM :: IO ()
mainM =
  putStrLn "First pizza size:"
    >> getLine
    >>= \size1 ->
      putStrLn "First pizza cost:"
        >> getLine
        >>= ( \cost1 ->
                putStrLn "Second pizza size:"
                  >> getLine
                  >>= \size2 ->
                    putStrLn "Second pizza cost:"
                      >> getLine
                      >>= \cost2 ->
                        putStrLn (describePizza (comparePizzas (read size1, read cost1) (read size2, read cost2)))
            )
