times :: Integer -> Integer -> Integer
times x y = x * y

plus :: Integer -> Integer -> Integer
plus x y = x + y

-- same_values :: Function -> Function -> Integer -> Integer -> Bool
same_values p1 p2 x y = (p1 x y == p2 x y)

main = do
   putStrLn "same_values for plus times 2 3 and 2 2"
   print(same_values plus times 2 3)
   print(same_values plus times 2 2)
