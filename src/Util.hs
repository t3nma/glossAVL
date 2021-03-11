{-

  Util.hs
  -------

  Well... The implementation of util functions.

-}

module Util
    ( readLoop
    ) where

import System.IO (isEOF)

-- Sort of a strict version of getContents
readLoop :: IO [String]
readLoop = do
  done <- isEOF
  if done
    then return ([])
    else do cur <- getLine
            next <- readLoop
            return (cur:next)
