data Tree a = Node a [Tree a]

-- sizeTree _ = 0;
-- sizeTree (Node _ tree) = 1 + (sizeTree tree)

main = do
    putStrLn "===> ..."
    -- print(...)