data Shape = Circle { radius :: Int }
           | Square { side :: Int }
           | Rectangle { width :: Int, height :: Int }

perimeter :: Shape -> Int
perimeter (Circle radius) = radius * 2
perimeter (Square side) = side * 4
--perimeter (Rectangle rectangle) = (width rectangle) + (height rectangle)
perimeter (Rectangle w h) = w + h


