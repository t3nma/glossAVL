module Main where

-- import Lib
import Graphics.Gloss

main :: IO ()
main = display window white teste

window = InWindow "GlossTeste" (800,600) (0,0)
teste = circleSolid 100
