substituirNegativos :: [Int] -> [Int]
substituirNegativos = map (\x -> if x < 0 then 1 else x)

ehPrimo :: Int -> Bool
ehPrimo n
  | n <= 1    = False
  | otherwise = null [x | x <- [2..(n-1)], n `mod` x == 0]

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)


testSubstituirNegativos :: Bool
testSubstituirNegativos =
  substituirNegativos [1, -2, 3, -4, 5] == [1, 1, 3, 1, 5]
  && substituirNegativos [0, 0, 0] == [0, 0, 0]
  && substituirNegativos [-1, -1, -1] == [1, 1, 1]

testEhPrimo :: Bool
testEhPrimo =
  ehPrimo 2 == True
  && ehPrimo 3 == True
  && ehPrimo 4 == False
  && ehPrimo 13 == True
  && ehPrimo 1 == False
  && ehPrimo 0 == False
  && ehPrimo (-5) == False

testFatorial :: Bool
testFatorial =
  fatorial 0 == 1
  && fatorial 1 == 1
  && fatorial 5 == 120
  && fatorial 7 == 5040

testAll :: Bool
testAll = testSubstituirNegativos && testEhPrimo && testFatorial
