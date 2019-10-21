fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
   putStrLn "Fibbonacci sequence from 5 to 10:"
   print(fib 5)
   print(fib 6)
   print(fib 7)
   print(fib 8)
   print(fib 9)
   print(fib 10)
