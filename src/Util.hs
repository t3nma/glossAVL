module Util
    ( Command (Add, Rem),
      toCmd,
      toCmdList
    ) where

data Command = Add Int | Rem Int
  deriving Show

toCmd :: String -> Command
toCmd str
  | op == '+' = Add key
  | otherwise = Rem key
  where
    op  = str !! 0
    key = read (drop 1 str) :: Int

toCmdList :: [String] -> [Command]
toCmdList []     = []
toCmdList (x:xs) = toCmd x:toCmdList xs
