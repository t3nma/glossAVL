module Main where

import GlossAVL

main :: IO ()
main = do
  model <- initModel :: IO (Model Int)
  run model
