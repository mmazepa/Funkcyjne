foldrr f e []     = e 
foldrr f e (x:xs) = f x (foldrr f e xs)

foldll f e []     = e                  
foldll f e (x:xs) = foldll f (f e x) xs

prod1 = foldrr (*) 1

length1 = foldrr (\_ n -> n+1) 0

and1 = foldrr (&&) True

nwd1 (x:xs) = foldrr gcd x xs

delete a = foldrr (\x xs -> if x == a then xs else (x:xs)) []

map1 f = foldrr (\x xs -> (f x):xs) []

reverse1 = foldrr (\ x acc -> acc ++ [x]) []

filter1 pred = foldrr (\x xs -> if pred x then x:xs else xs ) []

main = do
    putStrLn "===> prod1 [1,2,3,4]"
    print(prod1 [1,2,3,4])
    putStrLn "===> length1 [1,2,3,4,1,2,3,4]"
    print(length1 [1,2,3,4,1,2,3,4])
    putStrLn "===> and1 [True, True, True, False, True]"
    print(and1 [True, True, True, False, True])
    putStrLn "===> nwd1 [12,18,24,30]"
    print(nwd1 [12,18,24,30])
    putStrLn "===> delete 5 [1,2,3,4,5,6,7]"
    print(delete 5 [1,2,3,4,5,6,7])
    putStrLn "===> map1 sqrt [1,4,9,16,25,36,49]"
    print(map1 sqrt [1,4,9,16,25,36,49])
    putStrLn "===> reverse1 [1,2,3,4]"
    print(reverse1 [1,2,3,4])
    putStrLn "===> filter1 (>5) [1,2,3,4,5,6,7,8,9,10]"
    print(filter1 (>5) [1,2,3,4,5,6,7,8,9,10])