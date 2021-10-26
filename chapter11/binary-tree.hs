
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left v right) = Node (mapTree f left) (f v) (mapTree f right)


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left v right) = [v] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left v right) = inorder left ++ [v] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left v right) = postorder left ++ postorder right ++ [v]
