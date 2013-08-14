-- Task: Insert a list of [a] into a Tree using a fold. The resulting tree
-- should be balanced.
-- Status: FEELS SO GOOD
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
      deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert n Leaf = Node 0 Leaf n Leaf
insert n node@(Node h l v r)
  | perfectBalance node = Node (h+1) (insert n l) v r
  | perfectBalance l = Node h l v (insert n r)
  | otherwise = Node h (insert n l) v r

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

perfectBalance :: Tree a -> Bool
perfectBalance Leaf = True
perfectBalance (Node h l _ r) = (height l == height r) && (perfectBalance l) && (perfectBalance r)
