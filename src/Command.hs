{-

  Command.hs
  ----------

  Implementation of the Command data structure.
  A command represents an operation to execute
  on an AVL Tree.

  Supported operations:
    - Add key -- insert key
    - Rem key -- remove key

  Any type instantiating Read, Show and Ord can
  be used to create commands. Each command is
  associated with a symbol that is used to input
  the desired operations.

  Input notation example:
    $ +20 -- Add 20
    $ +50 -- Add 50
    $ -20 -- Rem 20

-}

module Command
  ( Command (Add, Rem),
    toCmd,
    toCmdList
  ) where

import Text.Read (readMaybe)

data Command a = Add a | Rem a
  deriving Show

-- Command parser.
parse :: (Read a, Show a, Ord a) => String -> Maybe (Command a)
parse str
  | op == '+' = parse' Add key
  | op == '-' = parse' Rem key
  | otherwise = Nothing
  where
    op  = head str
    key = readMaybe $ tail str
    --
    parse' cmd Nothing  = Nothing
    parse' cmd (Just v) = Just $ cmd v

-- Build a command from a string.
toCmd :: (Read a, Show a, Ord a) => String -> Maybe (Command a)
toCmd str = parse str

-- Build a list of commands from a list of strings.
toCmdList :: (Read a, Show a, Ord a) => [String] -> [Maybe (Command a)]
toCmdList []     = []
toCmdList (x:xs) = toCmd x:toCmdList xs
