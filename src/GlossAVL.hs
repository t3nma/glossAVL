{-

  GlossAVL.hs
  -----------

  AVL Tree visualization with Gloss.

  The layout being used places the nodes in the
  space according to their width (size of the subtree)
  and parent node location.

  Any type instantiating Show can be used in the
  model of this visualization. However, node and
  text sizes were choosed for trees storing Int's.
  All the layout constants can be changed.

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
screenSize = (1000,800)

window = InWindow "GlossAVL" screenSize (0,0)

widthFactor :: Float
widthFactor = 40

levelSpacing :: Float
levelSpacing = 50

topMargin :: Float
topMargin = 10

nodeRadius :: Float
nodeRadius = 20

leafSize :: Float
leafSize = 12

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

nodeLink :: Point -> Point -> Picture
nodeLink p1 p2 = line [p1,p2]

---
--- Auxiliary
---

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ _ l r) = 1 + treeSize l + treeSize r

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
    worker (x,y) (Node k h left right) pics = edge1:edge2:worker ptLeft left pics'
      where
        width   = fromIntegral $ treeSize (Node k h left right) :: Float
        ptLeft  = (x-width*widthFactor/2,y-levelSpacing)
        ptRight = (x+width*widthFactor/2,y-levelSpacing)
        pics'   = nodeAt k (x,y):worker ptRight right pics
        edge1   = nodeLink (x,y) ptLeft
        edge2   = nodeLink (x,y) ptRight

updateModel :: Ord a => ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[])     = (tree,[])
updateModel _ _ (tree,(x:xs)) = (doCmd x tree,xs)

-- Main.
run :: (Read a, Show a, Ord a) => Model a -> IO ()
run model = simulate window white 1 model drawModel updateModel
