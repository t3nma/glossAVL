{-
  Implementation of an AVL Tree data structure.
  This version stores each node's height in the node itself.
-}

module AVLTree (
  Tree,
  empty,
  find,
  insert,
  remove
  ) where

data Tree a
  = Empty
  | Node a Int (Tree a) (Tree a) -- (key, height, left, right)
  deriving Show

-- constructor
empty :: Tree a
empty = Empty

-- node height
height :: Tree a -> Int
height Empty = 0
height (Node _ h _ _) = h

-- height difference ("desvio")
hDiff :: Tree a -> Int
hDiff Empty = 0
hDiff (Node _ _ left right) = height left - height right

-- node retrieval
find :: Ord a => a -> Tree a -> Maybe (Tree a)
find a tree = worker tree
  where
    worker Empty = Nothing
    worker (Node a' h left right)
      = case compare a a' of
          EQ -> Just (Node a h left right)
          LT -> worker left
          GT -> worker right

-- single right rotation
rRotate :: Tree a -> Tree a
rRotate (Node a h (Node a' h' d1 d2) d3) = Node a' 0 d1 (Node a 0 d2 d3)
rRotate tree = tree

-- single left rotation
lRotate :: Tree a -> Tree a
lRotate (Node a h d1 (Node a' h' d2 d3)) = Node a' 0 (Node a 0 d1 d2) d3
lRotate tree = tree

-- right-balance a (sub)tree with height diff 2
rFix :: Tree a -> Tree a
rFix (Node a h d1 d2)
  | hDiff d1 == -1 = rRotate (Node a h (lRotate d1) d2)
  | otherwise      = rRotate (Node a h d1 d2)
rFix tree = tree

-- left-balance a (sub)tree with height diff -2
lFix :: Tree a -> Tree a
lFix (Node a h d1 d2)
  | hDiff d2 == 1 = lRotate (Node a h d1 (rRotate d2))
  | otherwise     = lRotate (Node a h d1 d2)
lFix tree = tree

-- balance tree with |height diff| = 2
fix :: Tree a -> Tree a
fix tree
  | d == 2    = hUpdate $ rFix tree
  | d == -2   = hUpdate $ lFix tree
  | otherwise = tree
  where
    d = hDiff tree

-- update (sub)tree height
hUpdate :: Tree a -> Tree a
hUpdate Empty = Empty
hUpdate (Node a h left right) = Node a h' left' right'
  where
    left'  = hUpdate left
    right' = hUpdate right
    h'     = max (height left') (height right') + 1

-- value insertion
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

-- value removal
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

-- maximum node retrieval
findMax :: Tree a -> Maybe (Tree a)
findMax Empty = Nothing
findMax (Node a h left Empty) = Just $ Node a h left Empty
findMax (Node _ _ _ right)    = findMax right
