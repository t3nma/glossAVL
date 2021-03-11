module Main where

import Util
import Command
import AVLTree
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Maybe (catMaybes)

type Model a = (Tree a, [Command a])

main :: IO ()
main = do
  model <- initModel :: IO (Model Int)
  simulate FullScreen white 1 model drawModel updateModel

initModel :: (Read a, Show a, Ord a) => IO (Model a)
initModel = do
  commands <- fmap (catMaybes . toCmdList) readLoop
  return (empty,commands)

drawModel :: (Show a) => Model a -> Picture
drawModel (_,[])         = text "END"
drawModel (_,(Add a:xs)) = text ("+" ++ show a)
drawModel (_,(Rem a:xs)) = text ("-" ++ show a)

updateModel :: ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[]) = (tree,[])
updateModel _ _ (tree,xs) = (tree,tail xs)
