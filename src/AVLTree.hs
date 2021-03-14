{-

  AVLTree.hs
  ----------

  Implementation of an AVL Tree data structure.
  This version stores each node's height in the
  nodes themselves.

-}

module AVLTree
  ( Tree (Empty, Node), -- export all just to avoid the need to make getters()
    empty,
    find,
    insert,
    remove
  ) where

data Tree a
  = Empty
  | Node a Int (Tree a) (Tree a) -- (key, height, left, right)
  deriving Show

-- Tree constructor.
empty :: Tree a
empty = Empty

-- Retrieve some node's height.
height :: Tree a -> Int
height Empty = 0
height (Node _ h _ _) = h

-- Retrieve some node's subtree height difference.
hDiff :: Tree a -> Int
hDiff Empty = 0
hDiff (Node _ _ left right) = height left - height right

-- Retrieve some node with the given key.
find :: Ord a => a -> Tree a -> Maybe (Tree a)
find a tree = worker tree
  where
    worker Empty = Nothing
    worker (Node a' h left right)
      = case compare a a' of
          EQ -> Just (Node a h left right)
          LT -> worker left
          GT -> worker right

-- Perform a single right rotation.
rRotate :: Tree a -> Tree a
rRotate (Node a h (Node a' h' d1 d2) d3) = Node a' 0 d1 (Node a 0 d2 d3) -- heights will be updated later
rRotate tree = tree

-- Perform a single left rotation.
lRotate :: Tree a -> Tree a
lRotate (Node a h d1 (Node a' h' d2 d3)) = Node a' 0 (Node a 0 d1 d2) d3 -- heights will be updated later
lRotate tree = tree

-- Perform a compound right rotation.
rFix :: Tree a -> Tree a
rFix (Node a h d1 d2)
  | hDiff d1 == -1 = rRotate (Node a h (lRotate d1) d2)
  | otherwise      = rRotate (Node a h d1 d2)
rFix tree = tree

-- Perform a compound left rotation.
lFix :: Tree a -> Tree a
lFix (Node a h d1 d2)
  | hDiff d2 == 1 = lRotate (Node a h d1 (rRotate d2))
  | otherwise     = lRotate (Node a h d1 d2)
lFix tree = tree

-- Perform tree balance.
fix :: Tree a -> Tree a
fix tree
  | d == 2    = hUpdate $ rFix tree
  | d == -2   = hUpdate $ lFix tree
  | otherwise = tree
  where
    d = hDiff tree

-- Update the height values of some given node's subtree.
hUpdate :: Tree a -> Tree a
hUpdate Empty = Empty
hUpdate (Node a h left right) = Node a h' left' right'
  where
    left'  = hUpdate left
    right' = hUpdate right
    h'     = max (height left') (height right') + 1

-- Insert a node with the given key in the tree.
insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Node a 1 Empty Empty
insert a (Node a' h' left right)
  = case compare a a' of
      EQ -> Node a h' left right
      LT -> fix $ Node a' hl left' right
      GT -> fix $ Node a' hr left right'
  where
    left'  = insert a left
    right' = insert a right
    hl     = max (height left') (height right) + 1
    hr     = max (height left) (height right') + 1

-- Remove a node with the given key from the tree.
remove :: Ord a => a -> Tree a -> Tree a
remove a Empty = Empty
remove a (Node a' h left right)
  = case compare a a' of
      EQ -> fix $ remove' left right
      LT -> fix $ Node a' hl left' right
      GT -> fix $ Node a' hr left right'
  where
    remove' left Empty  = left
    remove' Empty right = right
    remove' left right  = hUpdate $ Node a_max h_max (remove a_max left) right
      where
        Just (Node a_max h_max _ _) = findMax left
    left'  = remove a left
    right' = remove a right
    hl     = max (height left') (height right) + 1
    hr     = max (height left) (height right') + 1

-- Retrieve the node with maximum key from some
-- given node's subtree.
findMax :: Tree a -> Maybe (Tree a)
findMax Empty = Nothing
findMax (Node a h left Empty) = Just $ Node a h left Empty
findMax (Node _ _ _ right)    = findMax right
