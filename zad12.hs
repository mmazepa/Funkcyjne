foldrr f e []     = e 
foldrr f e (x:xs) = f x (foldrr f e xs)

foldll f e []     = e                  
foldll f e (x:xs) = foldll f (f e x) xs

main = do
   putStrLn "===> foldrr (+) 0 [1,2,3,4]"
   print(foldrr (+) 0 [1,2,3,4])
   putStrLn "===> foldrr (*) 1 [1,2,3,4]"
   print(foldrr (*) 1 [1,2,3,4])
   putStrLn "===> foldll (+) 0 [1,2,3,4]"
   print(foldll (+) 0 [1,2,3,4])
   putStrLn "===> foldll (*) 1 [1,2,3,4]"
   print(foldll (*) 1 [1,2,3,4])