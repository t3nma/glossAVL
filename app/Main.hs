module Main where

import Util
import AVLTree
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
-- import Graphics.Gloss.Interface.IO.Simulate

type Model a = (Tree a, [Command])

main :: IO ()
main = do
  model <- initModel
  -- simulateIO FullScreen white 60 model drawModel updateModel
  simulate FullScreen white 60 model drawModel updateModel

initModel :: IO (Model a)
initModel = do
  commands <- fmap (toCmdList . lines) getContents
  return (empty,commands)

{-
drawModel :: Model a -> IO Picture
drawModel (_,[])         = do return $ text "END."
drawModel (_,(Add a:xs)) = do return $ text ("+" ++ show a)
drawModel (_,(Rem a:xs)) = do return $ text ("-" ++ show a)

updateModel :: ViewPort -> Float -> Model a -> IO (Model a)
updateModel _ _ (tree,[])     = do return (tree,[])
updateModel _ _ (tree,(x:xs)) = do return (tree,xs)
-}

drawModel :: Model a -> Picture
drawModel (_,[])         = text "END."
drawModel (_,(Add a:xs)) = text ("+" ++ show a)
drawModel (_,(Rem a:xs)) = text ("-" ++ show a)

updateModel :: ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[])     = (tree,[])
updateModel _ _ (tree,(x:xs)) = (tree,xs)
