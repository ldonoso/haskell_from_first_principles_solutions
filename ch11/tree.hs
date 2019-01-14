module Tree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

--------------------------------------------------------------------------------
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node t1 x t2) = Node (mapTree f t1) (f x) (mapTree f t2)


testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

--------------------------------------------------------------------------------
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left v right) = v : (preorder left ++ preorder right)

inorder :: BinaryTree a -> [a]
inorder (Node left v right) = preorder left ++ (v : preorder right)

postorder :: Ord a => BinaryTree a -> [a]
postorder (Node left v right) = preorder left ++ preorder right ++ [v]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

--------------------------------------------------------------------------------
foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b 
foldTree f b (Node l v r) = f v (foldTree f b l) (foldTree f b r)

--------------------------------------------------------------------------------
mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree (\ v l r -> (Node l (f v) r)) Leaf bt
