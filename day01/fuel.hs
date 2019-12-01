main = do
  ls <- readFile "input_day01.txt"
  let massData = map asInt (lines ls)
      totalFuelNeeded = totalFuel (map calculateFuel massData)
  print totalFuelNeeded

asInt :: String -> Int
asInt x = read x :: Int

calculateFuel :: Int -> Int
calculateFuel mass 
    | fuelNeeded <= 0 = 0
    | otherwise = calculateFuel fuelNeeded + fuelNeeded
  where fuelNeeded = div mass 3 - 2

totalFuel :: [Int] -> Int
totalFuel fd = foldr (+) 0 fd