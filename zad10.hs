map2 f [] [] = []
map2 f (x:xs) (y:ys) = (f x y) : map2 f xs ys

---------------
-- version 1 --
---------------
-- pairing xs [] = []
-- pairing [] ys = []
-- pairing (x:xs) (y:ys) = (x,y) : pairing xs ys

---------------
-- version 2 --
---------------
-- pair x y = (x,y)
-- pairing (x:xs) (y:ys) = (x,y) : map2 pair xs ys

---------------
-- version 3 --
---------------
pairing = map2 (\ x y -> (x,y)) 

filter1 p xs = [x | x <- xs, p x]

main = do
   putStrLn "===> map2 (+) [1,2,3] [4,5,6]"
   print(map2 (+) [1,2,3] [4,5,6])
   putStrLn "===> pairing [1,2,3,4] ['a','b','c','d']"
   print(pairing [1,2,3,4] ['a','b','c','d'])
   putStrLn "===> filter1 (>5) [1,3,5,7,9]"
   print(filter1 (>5) [1,3,5,7,9])
