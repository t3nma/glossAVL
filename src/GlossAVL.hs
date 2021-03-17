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
nodeRadius = 20

leafSize :: Float
leafSize = 12

---
--- Picture utils
---

circleSolid_ :: Picture
circleSolid_ = circleSolid nodeRadius

nodeAt :: Show a => a -> Point -> Bool-> Picture
nodeAt k (x,y) test = translate x y $ pictures node
  where
    c = if test then cyan else yellow -- cyan acts as the highlight of an insertion command
    node = [color c $ circleSolid_,
            translate (-8) (-7) $ scale 0.1 0.1 (text $ show k)]

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

cmdText :: Show a => Maybe (Command a) -> String
cmdText Nothing  = " "
cmdText (Just a) = show a

---
--- Model functions
---

initModel :: (Read a, Show a, Ord a) => IO (Model a)
initModel = do
  commands <- fmap (catMaybes . toCmdList) readLoop
  return (empty,commands,Nothing)

drawModel :: (Ord a, Show a) => Model a -> Picture
drawModel (tree,_,prev) = pictures $ cmdPic:treePic
  where
    h       = fromIntegral (snd screenSize) :: Float
    orig    = (0,h/4)
    cmdPic  = translate (-30) (h/4+nodeRadius+20) $ scale 0.2 0.2 $ text (cmdText prev)
    treePic = drawTree orig tree prev

drawTree :: (Ord a, Show a) => Point -> Tree a -> Maybe (Command a) -> [Picture]
drawTree _ Empty _    = []
drawTree pt tree prev = worker pt tree prev []
  where
    worker pt Empty _ pics                             = leafAt pt:pics
    worker (x,y) (Node k h left right) (Just cmd) pics = edge1:edge2:worker ptLeft left prev pics'
      where
        width    = fromIntegral $ treeSize (Node k h left right) :: Float
        ptLeft   = (x-width*widthFactor/2,y-levelSpacing)
        ptRight  = (x+width*widthFactor/2,y-levelSpacing)
        isTarget = k == cmdKey cmd
        pics'    = nodeAt k (x,y) isTarget:worker ptRight right prev pics
        edge1    = nodeLink (x,y) ptLeft
        edge2    = nodeLink (x,y) ptRight

updateModel :: Ord a => ViewPort -> Float -> Model a -> Model a
updateModel _ _ (tree,[],prev)  = (tree,[],prev)
updateModel _ _ (tree,(x:xs),_) = (doCmd x tree,xs,Just x)

-- Main.
run :: (Read a, Show a, Ord a) => Model a -> IO ()
run model = simulate window white 1 model drawModel updateModel
