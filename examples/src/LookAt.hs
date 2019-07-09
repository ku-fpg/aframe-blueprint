{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}

module LookAt where

import Text.AFrame.DSL as DSL
import Text.AFrame.WebPage
import Data.Text (Text)

main :: IO ()
main = webPage "examples/look-at/index.html" 
  [ aframe "0.9.2"
  , unpkg "aframe-look-at-component" "0.8.x"
  , unpkg "aframe-animation-component" "3.2.5"
  ] $ scene $ do
  entity $ do
    attribute "look-controls" ()
    attribute "wasd-controls" ()
    id_ "look-cam"
    component "camera" ("userHeight: 1.6" :: Text)
  
  entity $ do
    id_ "container"
    position (0,0,-4)
    sphere $ do
      color "#404040"
      radius 0.5
      id_ "target"
      animation position (8,-3,-2) (-8,6,-8) $ do
        dir "alternate"
        dur 1500
        loop Nothing
    
    entity $ do
      rotation (0,70,0)
      position (5,3,0)
      box $ do
        look_at "#target"
        height 1
        color "#01A1F1"
        width 1
        depth 1
        position (-2,0,-4)
      
    
    box $ do
      look_at "#target"
      height 1
      color "#4CC3D9"
      width 1
      depth 1
      position (-4,0,-2)
    
    cylinder $ do
      look_at "#target"
      height 2
      color "#7BC8A4"
      radius 0.6
      position (0,0,-2)
    
    box $ do
      look_at "#target"
      height 2
      color "#F16745"
      width 0.5
      depth 1
      position (4,0,-2)
    
    cylinder $ do
      look_at "#target"
      height 2
      color "#7BC8A4"
      radius 0.2
      position (-4,0,1)
    
    box $ do
      look_at "#target"
      height 0.25
      color "#93648D"
      width 2
      depth 1
      position (0,0,1)
    
    box $ do
      look_at "#target"
      height 0.5
      color "#999"
      width 1
      depth 2
      position (4,0,1)
    
    box $ do
      look_at "#look-cam"
      height 2
      color "#FFC65D"
      width 2
      depth 2
      position (-6,2.5,-2)
    
  
  sky $ do
    color "#ECECEC"
