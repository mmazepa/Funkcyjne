smaller :: Integer -> Integer -> Bool
smaller x y = x < y

greater :: Integer -> Integer -> Bool
greater x y = x > y

equal :: Integer -> Integer -> Bool
equal x y = not(smaller x y) && not(greater x y)

smaller_equal :: Integer -> Integer -> Bool
smaller_equal x y = not(greater x y)

greater_equal :: Integer -> Integer -> Bool
greater_equal x y = not(smaller x y)

not_equal :: Integer -> Integer -> Bool
not_equal x y = not(equal x y)

main = do
   putStrLn "===> smaller 1 2 and 2 1"
   print(smaller 1 2)
   print(smaller 2 1)
   putStrLn "===> greater 1 2 and 2 1"
   print(greater 1 2)
   print(greater 2 1)
   putStrLn "===> equal 1 2 and 2 2"
   print(equal 1 2)
   print(equal 2 2)
   putStrLn "===> smaller_equal 2 2 and 3 2"
   print(smaller_equal 2 2)
   print(smaller_equal 3 2)
   putStrLn "===> greater_equal 2 2 and 1 2"
   print(greater_equal 2 2)
   print(greater_equal 1 2)
   putStrLn "===> not_equal 1 2 and 2 2"
   print(not_equal 1 2)
   print(not_equal 2 2)
