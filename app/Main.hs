module Main where

import Util
import AVLTree
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified System.IO.Strict as SIO (getContents)

type Model a = (Tree a, [Command])

main :: IO ()
main = do
  model <- initModel
  simulate FullScreen white 1 model drawModel updateModel

initModel :: IO (Model a)
initModel = do
  commands <- fmap (toCmdList . lines) SIO.getContents
  return (empty,commands)

drawModel :: Model a -> Picture
drawModel (_,[])         = text "END"
drawModel (_,(Add a:xs)) = text ("+" ++ show a)
drawModel (_,(Rem a:xs)) = text ("-" ++ show a)

updateModel :: ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[])  = (tree,[])
updateModel _ _ (tree,xs) = (tree,tail xs)
