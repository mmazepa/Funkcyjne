even1 :: Integer -> Bool
even1 0 = True
even1 1 = False
even1 n = odd1(n-1)

odd1 :: Integer -> Bool
odd1 0 = False
odd1 1 = True
odd1 n = even1(n-1)

main = do
   putStrLn "===> even1 for 0 1 2 3 4 5"
   print(even1 0)
   print(even1 1)
   print(even1 2)
   print(even1 3)
   print(even1 4)
   print(even1 5)
   putStrLn "===> odd1 for 0 1 2 3 4 5"
   print(odd1 0)
   print(odd1 1)
   print(odd1 2)
   print(odd1 3)
   print(odd1 4)
   print(odd1 5)
