{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Text.AFrame
import Text.AFrame.WebPage
import Text.AFrame.DSL 

import System.Environment

example :: AFrame
example = scene $ do

  sphere $ do
    position (0,1.25,-1)
    radius   1.25
    color    "#EF2D5E"

  box $ do
    position (-1,0.5,1)
    rotation (0,45,0)
    width    1
    height   1
    scale   (1,1,1)
    color   "#4CC3D9"

  cylinder $ do
    position (1,0.75,1) 
    radius   0.5
    height   1.5
    color    "#FFC65D"
  
  plane $ do
    rotation (-90,0,0)
    width  4
    height 4
    color "#7BC8A4"

  sky $ color "#ECECEC"
  
  entity $ do
    position (0,0,5.8)
    camera $ return ()

main :: IO ()
main = do
        args <- getArgs
        main2 args
        

main2 :: [String] -> IO ()
main2 ("hello-world":args) = webPage args example
main2 _ = error "usage: aframe-blueprint-example example-name output.html"
