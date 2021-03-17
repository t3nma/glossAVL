{-

  GlossAVL.hs
  -----------

  AVL Tree visualization with Gloss.

  The layout being used places the nodes in the
  space according to their width (size of the subtree)
  and parent node location. Therefore, there's no
  control of whether the tree is getting out of
  the window bounds. You're expected to manually
  control the viewport in such cases.

  Any type instantiating Show can be used in the
  model of this visualization. However, node and
  text sizes were selected for trees storing Int's
  up to 4 digits.

  All the layout constants can be tuned.

-}

module GlossAVL
  ( Model,
    initModel,
    run
  ) where

import Util
import Command
import AVLTree
import Data.Maybe (catMaybes, fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Simulate

-- State = (AVL Tree, Input commands, Previous applied command)
type Model a = (Tree a,[Command a],Maybe (Command a))

---
--- Constants
---

screenSize :: (Int,Int)
screenSize = (1000,800)

window = InWindow "GlossAVL" screenSize (0,0)

widthFactor :: Float
widthFactor = 30

levelSpacing :: Float
levelSpacing = 65

nodeRadius :: Float
nodeRadius = 25

nodeThickness :: Float
nodeThickness = 1

nodeColor :: Color
nodeColor = yellow

nodeActiveColor :: Color
nodeActiveColor = cyan

leafSize :: Float
leafSize = 15

leafColor :: Color
leafColor = black

rootOrig :: Point
rootOrig = (0,800/4)

---
--- Picture utils
---

-- Retrieve picture of node at given point.
treeNodeAt :: Show a => a -> Point -> Bool-> Picture
treeNodeAt k (x,y) isTarget = translate x y $ pictures node
  where
    c    = if isTarget then nodeActiveColor else nodeColor -- highlight of a performed command
    node = [circleSolid (nodeRadius+nodeThickness),
            color c $ circleSolid nodeRadius,
            translate (-12) (-7) $ scale 0.1 0.1 (text $ show k)]

-- Retrieve picture of leaf node at given point.
leafAt :: Point -> Picture
leafAt (x,y) = translate x y $ color leafColor $ rectangleSolid leafSize leafSize

-- Retrieve picture of a line between two node points.
treeNodeLink :: Point -> Point -> Picture
treeNodeLink p1 p2 = line [p1,p2]

-- Retrieve picture of last command applied.
cmdLegend :: Show a => Maybe (Command a) -> Picture
cmdLegend cmd = translate (-35) y' $ scale 0.2 0.2 $ text (cmdText cmd)
  where
    (_,y) = rootOrig
    y'    = y+nodeRadius+20 -- 20px above root node

---
--- Auxiliary
---

-- Retrieve size for some given tree node.
treeSize :: Tree a -> Int
treeSize (Node _ _ l r) = 1 + treeSize l + treeSize r
treeSize _              = 0

-- Retrieve textual representation for a Maybe (Command).
cmdText :: Show a => Maybe (Command a) -> String
cmdText Nothing  = " "
cmdText (Just a) = show a

-- Retrieve some node's child points in the space.
childOrig :: Tree a -> Point -> (Point,Point)
childOrig node (x,y) = (ptLeft,ptRight)
  where
    width   = fromIntegral $ treeSize node :: Float
    ptLeft  = (x-width*widthFactor/2,y-levelSpacing)
    ptRight = (x+width*widthFactor/2,y-levelSpacing)

-- Retrieve [Picture] representation of a given tree state,
-- starting at the given origin point and considering the
-- given last applied command.
drawTree :: (Ord a, Show a) => Point -> Tree a -> Maybe (Command a) -> [Picture]
drawTree _ Empty _      = []
drawTree orig tree prev = worker orig tree prev []
  where
    worker pt Empty _ pics                          = leafAt pt:pics
    worker pt (Node k h left right) (Just cmd) pics = edge1:edge2:worker ptLeft left prev pics'
      where
        (ptLeft,ptRight) = childOrig (Node k h left right) pt
        edge1            = treeNodeLink pt ptLeft
        edge2            = treeNodeLink pt ptRight
        curNode          = treeNodeAt k pt (k == cmdKey cmd)
        pics'            = curNode:worker ptRight right prev pics

---
--- Model functions
---

-- Read input commands and retrieve model zero.
initModel :: (Read a, Show a, Ord a) => IO (Model a)
initModel = do
  commands <- fmap (catMaybes . toCmdList) readLoop
  return (empty,commands,Nothing)

-- Function that draws the model in each iteration.
drawModel :: (Ord a, Show a) => Model a -> Picture
drawModel (tree,_,prev) = pictures $ cmdPic:treePic
  where
    cmdPic  = cmdLegend prev
    treePic = drawTree rootOrig tree prev

-- Function that updates the model in each iteration.
updateModel :: Ord a => ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[],prev)  = (tree,[],prev)
updateModel _ _ (tree,(x:xs),_) = (doCmd x tree,xs,Just x)

-- Main.
run :: (Read a, Show a, Ord a) => Model a -> IO ()
run model = simulate window white 1 model drawModel updateModel
