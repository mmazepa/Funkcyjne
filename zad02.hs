gcd2 :: Integer -> Integer -> Integer
gcd2 n 0 = n
gcd2 n m = gcd2 m (mod n m)

main = do
   putStrLn "gcd2 for 21 and 14"
   print(gcd2 21 14)
   putStrLn "gcd2 for 18 and 12"
   print(gcd2 18 12)
   putStrLn "gcd2 for 15 and 10"
   print(gcd2 15 10)
