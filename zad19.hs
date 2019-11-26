map1 f []     = []
map1 f (x:xs) = (f x) : (map1 f xs)

append []     l = l
append (x:xs) l = x : (append xs l)

sum1 f []     = 0
sum1 f (x:xs) = (f x) + (f xs)

member x []     = False
member x (y:ys) = (x == y) || (member x ys)


main = do
    putStrLn "===> map1 square [1,2,3,4,5]"
    print(map1 square [1,2,3,4,5])
    putStrLn "===> append [1,2,3] 4"
    print(append [1,2,3] 4)
    putStrLn "===> sum1 square [1,2,3,4]"
    print(sum1 square [1,2,3,4])
    putStrLn "===> member 5 [3,4,5,6,7]"
    print(member 5 [3,4,5,6,7])