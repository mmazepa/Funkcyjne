append [] m = m
append (x:xs) m = x:(append xs m)

member x [] = False
member x (y:ys) = (y == x || member x ys)

reverse1 [] = []
reverse1 (x:xs) = append (reverse1 xs) [x]

last1 [] = []
last1 [x] = [x]
last1 (x:xs) = last1 xs

delete1 _ [] = []
delete1 x (y:ys)
   | (x == y) = delete1 x ys
   | otherwise = y : delete1 x ys

-- split x l

map1 f [] = []
map1 f (x:xs) = (f x) : map1 f xs

main = do
   putStrLn "===> append [4,5] [1,2,3]"
   print(append [4,5] [1,2,3])
   putStrLn "===> member 1 [1,2,3]"
   print(member 1 [1,2,3])
   putStrLn "===> member 4 [1,2,3]"
   print(member 4 [1,2,3])
   putStrLn "===> reverse1 [1,2,3,4,5,6,7,8,9,10]"
   print(reverse1 [1,2,3,4,5,6,7,8,9,10])
   putStrLn "===> last1 [3,4,5,6,7]"
   print(last1 [3,4,5,6,7])
   putStrLn "===> delete1 4 [2,3,4,5,6,7]"
   print(delete1 4 [2,3,4,5,6,7])
   putStrLn "===> map1 sqrt [1,4,9,16,25,36,49,64,81,100]"
   print(map1 sqrt [1,4,9,16,25,36,49,64,81,100])
