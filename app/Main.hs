module Main where

-- import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment

fps :: Int
fps = 60

type ScreenSize = (Int, Int)
type Text = (ScreenSize, Int)

initNum :: IO Text
initNum = do
  size <- getScreenSize -- didn't really use it but now I know how to do it
  return (size, 0)

main :: IO ()
main = do
  num <- initNum
  simulate FullScreen white fps num drawNum updateNum

drawNum :: Text -> Picture
drawNum (_,n) = pictures [circleSolid 200,
                          translate (-50.0) (-25.0) $ scale 0.5 0.5 $ color white (text $ show n)]

updateNum :: ViewPort -> Float -> Text -> Text
updateNum _ _ (size,n) = (size,n+1)

-- window = InWindow "GlossTeste" (800,600) (0,0)
-- teste = circleSolid 100
