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
h_diff :: Tree a -> Int
h_diff Empty = 0
h_diff (Node _ _ left right) = height left - height right

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
r_rotate :: Tree a -> Tree a
r_rotate (Node a h (Node a' h' d1 d2) d3) = Node a' 0 d1 (Node a 0 d2 d3)
r_rotate tree = tree

-- single left rotation
l_rotate :: Tree a -> Tree a
l_rotate (Node a h d1 (Node a' h' d2 d3)) = Node a' 0 (Node a 0 d1 d2) d3
l_rotate tree = tree

-- right-balance a (sub)tree with height diff 2
r_fix :: Tree a -> Tree a
r_fix (Node a h d1 d2)
  | h_diff d1 == -1 = r_rotate (Node a h (l_rotate d1) d2)
  | otherwise       = r_rotate (Node a h d1 d2)
r_fix tree = tree

-- left-balance a (sub)tree with height diff -2
l_fix :: Tree a -> Tree a
l_fix (Node a h d1 d2)
  | h_diff d2 == 1 = l_rotate (Node a h d1 (r_rotate d2))
  | otherwise      = l_rotate (Node a h d1 d2)
l_fix tree = tree

-- balance tree with |height diff| = 2
fix :: Tree a -> Tree a
fix tree
  | d == 2    = h_update $ r_fix tree
  | d == -2   = h_update $ l_fix tree
  | otherwise = tree
  where
    d = h_diff tree

-- update (sub)tree height
h_update :: Tree a -> Tree a
h_update Empty = Empty
h_update (Node a h left right) = Node a h' left' right'
  where
    left'  = h_update left
    right' = h_update right
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
    remove' left right  = h_update $ Node a_max h_max (remove a_max left) right
      where
        Just (Node a_max h_max _ _) = find_max left
    left'  = remove a left
    right' = remove a right
    hl     = max (height left') (height right) + 1
    hr     = max (height left) (height right') + 1

-- maximum node retrieval
find_max :: Tree a -> Maybe (Tree a)
find_max Empty = Nothing
find_max (Node a h left Empty)  = Just $ Node a h left Empty
find_max (Node _ _ _ right)     = find_max right
