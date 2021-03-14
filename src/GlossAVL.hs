{-

  GlossAVL.hs
  -----------

-}

module GlossAVL
  ( Model,
    initModel,
    run
  ) where

import Util
import Command
import AVLTree
import Data.Maybe (catMaybes)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Simulate

type Model a = (Tree a,[Command a])

---
--- Constants
---

screenSize :: (Int,Int)
screenSize = (800,600)

window = InWindow "GlossAVL" screenSize (0,0)

widthFactor :: Float
widthFactor = 40

heightFactor :: Float
heightFactor = 50

topMargin :: Float
topMargin = 10

nodeRadius :: Float
nodeRadius = 15

leafSize :: Float
leafSize = 10

---
--- Picture utils
---

circleSolid_ :: Picture
circleSolid_ = circleSolid nodeRadius

nodeAt :: Show a => a -> Point -> Picture
nodeAt txt (x,y) = translate x y $ pictures node
  where
    node = [color yellow $ circleSolid_,
            translate (-8) (-7) $ scale 0.1 0.1 (text $ show txt)]

rectangleSolid_ :: Picture
rectangleSolid_ = rectangleSolid leafSize leafSize

leafAt :: Point -> Picture
leafAt (x,y) = translate x y $ rectangleSolid_

---
--- Model functions
---

initModel :: (Read a, Show a, Ord a) => IO (Model a)
initModel = do
  commands <- fmap (catMaybes . toCmdList) readLoop
  return (empty,commands)

drawModel :: Show a => Model a -> Picture
drawModel (tree,_) = pictures $ drawTree orig tree
  where
    h    = fromIntegral (snd screenSize) :: Float
    orig = (0,h/2-nodeRadius-topMargin)

drawTree :: Show a => Point -> Tree a -> [Picture]
drawTree _ Empty = []
drawTree pt tree = worker pt tree []
  where
    worker pt Empty pics                    = leafAt pt:pics
    worker (x,y) (Node k h left right) pics = worker ptLeft left pics'
      where
        width   = fromIntegral $ treeSize (Node k h left right) :: Float
        ptLeft  = (x-width*widthFactor/2,y-heightFactor)
        ptRight = (x+width*widthFactor/2,y-heightFactor)
        pics'   = nodeAt k (x,y):worker ptRight right pics

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ _ l r) = 1 + treeSize l + treeSize r

updateModel :: Ord a => ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[])     = (tree,[])
updateModel _ _ (tree,(x:xs)) = (doCmd x tree,xs)

-- Main.
run :: (Read a, Show a, Ord a) => Model a -> IO ()
run model = simulate window white 1 model drawModel updateModel
