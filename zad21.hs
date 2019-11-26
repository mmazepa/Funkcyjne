drop1 _ [] = Nothing
drop1 0 ls = Just ls
drop1 x ls = drop1 (x-1) (tail ls)

plus (Just x) Nothing = Just x
plus (Just x) (Just y) = Just (x+y)

sum1 [] = Nothing
sum1 (x:xs) = plus (Just x) (sum1 xs)

main = do
    putStrLn "===> drop1 5 [1,2,3,4,5,6,7,8,9,10]"
    print(drop1 5 [1,2,3,4,5,6,7,8,9,10])
    putStrLn "===> drop1 5 [1,2,3,4,5]"
    print(drop1 5 [1,2,3,4,5])
    putStrLn "===> sum1 [Just 1, Just 2, Just 3, Just 4]"
    print(sum1 [Just 1, Just 2, Just 3, Just 4])