data BinTree a = Leaf a | Node a (BinTree a) (BinTree a) deriving Show

heightBinTree (Leaf _) = 1
heightBinTree (Node _ left right) = 1 + (max (heightBinTree left) (heightBinTree right))

sizeBinTree (Leaf _) = 1
sizeBinTree (Node _ left right) = 1 + (sizeBinTree left) + (sizeBinTree right)

maxBinTree (Leaf x) = x
maxBinTree (Node x left right) = max x (max (maxBinTree left) (maxBinTree right))

preBinTree (Leaf x) = [x]
preBinTree (Node x left right) = [x] ++ preBinTree left ++ preBinTree right

mapBinTree f (Leaf x) = Leaf (f x)
mapBinTree f (Node x left right) = Node (f x) (mapBinTree f left) (mapBinTree f right)

foldBinTree f g (Leaf x) = f x
foldBinTree f g (Node x left right) = g x (foldBinTree f g left) (foldBinTree f g right)

-- heightBinTree2 (Node _ left right) =

main = do
    putStrLn "===> heightBinTree(Leaf 1)"
    print(heightBinTree(Leaf 1))
    putStrLn "===> heightBinTree(Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Leaf 7)))"
    print(heightBinTree(Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Leaf 7))))
    putStrLn "===> sizeBinTree(Leaf 1)"
    print(sizeBinTree(Leaf 1))
    putStrLn "===> sizeBinTree(Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Leaf 7)))"
    print(sizeBinTree(Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Leaf 7))))
    putStrLn "===> maxBinTree(Leaf 5)"
    print(maxBinTree(Leaf 5))
    putStrLn "===> maxBinTree(Node 5 (Node 2 (Leaf 4) (Leaf 7)) (Node 8 (Leaf 12) (Leaf 10)))"
    print(maxBinTree(Node 5 (Node 2 (Leaf 4) (Leaf 7)) (Node 8 (Leaf 12) (Leaf 10))))
    putStrLn "===> preBinTree(Leaf 5)"
    print(preBinTree(Leaf 5))
    putStrLn "===> preBinTree(Node 5 (Node 2 (Leaf 4) (Leaf 7)) (Node 8 (Leaf 12) (Leaf 10)))"
    print(preBinTree(Node 5 (Node 2 (Leaf 4) (Leaf 7)) (Node 8 (Leaf 12) (Leaf 10))))