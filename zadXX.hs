member x [] = False
member x (y:ys) = (y == x || member x ys)

member2 x [] = False
member2 x (l:ls) =
  if x == l then True
  else member2 x ls

delete1 _ [] = []
delete1 x (l:ls) =
   if x == l then delete1 x ls
   else l : delete1 x ls

exists pred [] = False
exists pred (l:ls) =
  if pred l then True
  else exists pred ls

cut_and_reverse [] = []
cut_and_reverse [x] = []
cut_and_reverse (x:xs) = reverse (xs)

cut l = cut_and_reverse (cut_and_reverse l)

palindrom [] = True
palindrom [x] = True
palindrom l = l == reverse l

palindrom2 [] = True
palindrom2 [x] = True
palindrom2 list@(l:ls) =
  (if l == last ls then True else False) && palindrom2 (cut list)

append [] m = m
append (l:ls) m = l : (append ls m)

reverse1 [] = []
reverse1 (l:ls) = append (reverse1 ls) [l]

-- split x [] = ([], [])
-- split x l:ls = split x (filter (>x) l, filter (not . (>x)) l)

data Tree a b = Leaf a | Node a b (Tree a b) (Tree a b) deriving Show

sumTree (Leaf a) = a
sumTree (Node a _ left right) = a + (sumTree left) + (sumTree right)

preTree (Leaf a) = [a]
preTree (Node a _ left right) = [a] ++ preTree left ++ preTree right

inTree (Leaf a) = [a]
inTree (Node a _ left right) = inTree left ++ [a] ++ inTree right

postTree (Leaf a) = [a]
postTree (Node a _ left right) = postTree left ++ postTree right ++ [a]

square x = x * x

mapb f (Leaf a) = (Leaf a)
mapb f (Node a b left right) = Node a (f b) (mapb f left) (mapb f right)

map1 f l = [f x | x <- l]

-- fibb x =
--   if (x < 2) then 1
--   else fibb (x-1) + fibb (x-2)

-- fibb x
--   | x == 0 = 0
--   | x == 1 = 1
--   | x >= 2 = fibb (x-1) + fibb (x-2)

fibb 0 = 0
fibb 1 = 1
fibb x = fibb (x-1) + fibb (x-2)

nfibbs_h 0 = []
nfibbs_h n = fibb(n) : nfibbs_h(n-1)
nfibbs n = reverse(nfibbs_h n)

anotherMap f [] = []
anotherMap f (l:ls) = (f l):(anotherMap f ls)

inc x = x + 1
four_incs x = (inc . inc . inc . inc) x

double x = x * 2
four_doubles x = (double . double . double . double) x

main = do
  putStrLn ""
  putStrLn "===> member 1 [1,2,3]"
  print(member 1 [1,2,3])
  putStrLn "===> member 4 [1,2,3]"
  print(member 4 [1,2,3])
  putStrLn "===> member2 1 [1,2,3]"
  print(member2 1 [1,2,3])
  putStrLn "===> member2 4 [1,2,3]"
  print(member2 4 [1,2,3])
  putStrLn ""
  putStrLn "===> delete1 2 [1,2,3]"
  print(delete1 2 [1,2,3])
  putStrLn "===> delete1 2 [1,2,3,2,1,2,3,2,1,2,3,5,2]"
  print(delete1 2 [1,2,3,2,1,2,3,2,1,2,3,5,2])
  putStrLn ""
  putStrLn "===> exists (>5) [1,2,3,4]"
  print(exists (>5) [1,2,3,4])
  putStrLn "===> exists (>5) [3,4,5,6,7]"
  print(exists (>5) [3,4,5,6,7])
  putStrLn ""
  putStrLn "===> cut [1,2,3,4]"
  print(cut [1,2,3,4])
  putStrLn "===> cut [4,3,2,0,1,2]"
  print(cut [4,3,2,0,1,2])
  putStrLn ""
  putStrLn "===> palindrom [1,1,2,4,4,1]"
  print(palindrom [1,1,2,4,4,1])
  putStrLn "===> palindrom [1,2,3,2,1]"
  print(palindrom [1,2,3,2,1])
  putStrLn "===> palindrom2 [1,1,2,4,4,1]"
  print(palindrom2 [1,1,2,4,4,1])
  putStrLn "===> palindrom2 [1,2,3,2,1]"
  print(palindrom2 [1,2,3,2,1])
  putStrLn ""
  putStrLn "===> append [1,2,3] [4,5,6]"
  print(append [1,2,3] [4,5,6])
  putStrLn ""
  putStrLn "===> reverse1 [1,2,3,4,5,6,7]"
  print(reverse1 [1,2,3,4,5,6,7])
  putStrLn ""
  putStrLn "===> sumTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2)))"
  print(sumTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2))))
  putStrLn ""
  putStrLn "===> preTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2)))"
  print(preTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2))))
  putStrLn ""
  putStrLn "===> inTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2)))"
  print(inTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2))))
  putStrLn ""
  putStrLn "===> postTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2)))"
  print(postTree(Node 5 0 (Node 2 0 (Leaf 4) (Leaf 7)) (Node 8 0 (Leaf 2) (Leaf 2))))
  putStrLn ""
  putStrLn "===> mapb square (Node 5 5 (Node 2 2 (Leaf 4) (Leaf 7)) (Node 8 8 (Leaf 2) (Leaf 2)))"
  print(mapb square (Node 5 5 (Node 2 2 (Leaf 4) (Leaf 7)) (Node 8 8 (Leaf 2) (Leaf 2))))
  putStrLn ""
  putStrLn "===> map1 square [1,2,3,4,5]"
  print(map1 square [1,2,3,4,5])
  putStrLn ""
  putStrLn "===> fibb 10"
  print(fibb 10)
  putStrLn "===> nfibbs 10"
  print(nfibbs 10)
  putStrLn ""
  putStrLn "===> anotherMap square [1,2,3,4,5]"
  print(anotherMap square [1,2,3,4,5])
  putStrLn ""
  putStrLn "===> inc 10"
  print(inc 10)
  putStrLn "===> four_incs 10"
  print(four_incs 10)
  putStrLn ""
  putStrLn "===> double 10"
  print(double 10)
  putStrLn "===> four_doubles 10"
  print(four_doubles 10)
  putStrLn ""
