append [] m = m
append (l:ls) m = l : (append ls m)

flatten [] = []
flatten (l:ls) = append l (flatten ls)

-- member x [] = False
-- member x (l:ls) =
--   if x == l then True
--   else (member x ls)

member x [] = False
member x (l:ls) = x == l || (member x ls)

take_while p [] = []
take_while p (l:ls) =
  if (p l) then l : (take_while p ls)
  else (take_while p ls)

main = do
  putStrLn ""
  putStrLn "===> append [1,2,3] [1,2,3]"
  print(append [1,2,3] [1,2,3])
  putStrLn "===> append [4,5,6] [7,8]"
  print(append [4,5,6] [7,8])
  putStrLn ""
  putStrLn "===> flatten [[1,2,3],[5],[7,8],[10]]"
  print(flatten [[1,2,3],[5],[7,8],[10]])
  putStrLn ""
  putStrLn "===> member 2 [1,2,3]"
  print(member 2 [1,2,3])
  putStrLn "===> member 1 [7,8]"
  print(member 1 [7,8])
  putStrLn ""
  putStrLn "===> take_while (>5) [1,2,5,6,3,4,10,0,0,12,2,3,6]"
  print(take_while (>5) [1,2,5,6,3,4,10,0,0,12,2,3,6])
  putStrLn ""
